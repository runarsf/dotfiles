_: {
  flake.nixosModules.host =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib)
        mkDefault
        mkIf
        mkMerge
        mkOption
        types
        ;
    in
    {
      options.host = {
        desktop = mkOption {
          type = types.bool;
          default = true;
          description = "Whether this host has a GUI session. GUI-only modules gate on this.";
        };

        efi = mkOption {
          type = types.bool;
          default = true;
          description = "Boot with systemd-boot. Disable on hosts that use grub.";
        };
      };

      config = mkMerge [
        (mkIf config.host.efi {
          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;
        })

        {
          # Needed to rebuild at all, and to not build everything from
          # source. The rest of the nix settings are in the `nix` module.
          nix.settings = rec {
            warn-dirty = false;
            experimental-features = [
              "nix-command"
              "flakes"
              "pipe-operators"
            ];
            substituters = [
              "https://cache.nixos.org/"
              "https://nix-community.cachix.org"
            ];
            trusted-substituters = substituters;
            trusted-public-keys = [
              "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
              "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
            ];
          };

          boot.kernelPackages = mkDefault pkgs.linuxPackages_latest;

          # Don't gate this on services.tlp.enable: nixos-hardware's laptop
          # profile already defaults tlp to off when this is on, and reading it
          # here closes the loop.
          services.power-profiles-daemon.enable = mkDefault config.host.desktop;
        }
      ];
    };
}
