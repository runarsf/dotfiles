{ self, lib', ... }:
{
  flake.nixosModules.home-assistant =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      cfg = config.modules.home-assistant;
      nginxDomains = config.modules.nginx.domains;
      usingNginx = nginxDomains != [ ];
      baseDomain = lib'.nginx.baseDomain nginxDomains cfg.domain;
    in
    {
      imports = [
        (lib'.importModule self "nixos" "sops")
        (lib'.importModule self "nixos" "nginx")
      ];

      options.modules.home-assistant.domain = lib.mkOption {
        type = lib.types.str;
        default =
          if nginxDomains != [ ] then
            "home.${builtins.head nginxDomains}"
          else
            throw "Set modules.home-assistant.domain or configure modules.nginx.domains";
        description = "Public domain for Home Assistant.";
      };

      config = lib.mkMerge [
        {
          sops = {
            secrets = {
              "home-assistant/latitude" = { };
              "home-assistant/longitude" = { };
            };

            templates."home-assistant-secrets.yaml" = {
              owner = "hass";
              restartUnits = [ "home-assistant.service" ];
              content = ''
                latitude: ${config.sops.placeholder."home-assistant/latitude"}
                longitude: ${config.sops.placeholder."home-assistant/longitude"}
              '';
            };
          };

          services.home-assistant = {
            enable = true;

            extraComponents = [
              # required for onboarding
              "analytics"
              "google_translate"
              "met"
              "radio_browser"
              "shopping_list"
              "isal"
              # your integrations
              "esphome"
              "mqtt"
              "zha"
              "hue"
              "swiss_public_transport"
            ];

            config = {
              default_config = { };
              homeassistant = {
                name = "Home";
                unit_system = "metric";
                time_zone = "Europe/Zurich";
                # lat/long/elevation via !secret
                latitude = "!secret latitude";
                longitude = "!secret longitude";
              };

              # Keep UI-created automations/scenes/scripts working alongside Nix-defined ones
              "automation ui" = "!include automations.yaml";
              "scene ui" = "!include scenes.yaml";
              "script ui" = "!include scripts.yaml";

              "automation nixos" = [
                # declarative automations here
              ];
            };

            customComponents = with pkgs.home-assistant-custom-components; [
              uix
            ];
            customLovelaceModules = with pkgs.home-assistant-custom-lovelace-modules; [
              mushroom
            ];
          };

          networking.firewall.allowedTCPPorts = [ 8123 ];

          systemd.tmpfiles.rules = [
            "f ${config.services.home-assistant.configDir}/automations.yaml 0644 hass hass"
            "f ${config.services.home-assistant.configDir}/scenes.yaml 0644 hass hass"
            "f ${config.services.home-assistant.configDir}/scripts.yaml 0644 hass hass"
            "L+ ${config.services.home-assistant.configDir}/secrets.yaml - - - - ${
              config.sops.templates."home-assistant-secrets.yaml".path
            }"
          ];
        }

        (lib.mkIf usingNginx {
          services.home-assistant.config.homeassistant.external_url = "https://${cfg.domain}";

          # HA 2026.11 ignores `http:` in YAML once migrated to its own store,
          # so use_x_forwarded_for/trusted_proxies are set in the UI instead.
          services.nginx.virtualHosts.${cfg.domain} = lib'.nginx.wildcardVhost baseDomain // {
            locations."/" = {
              proxyPass = "http://127.0.0.1:8123";
              proxyWebsockets = true;
            };
          };
        })

        (lib.mkIf (!usingNginx) {
          security.acme = {
            acceptTerms = true;
            certs.${cfg.domain} = { };
          };
        })
      ];
    };
}
