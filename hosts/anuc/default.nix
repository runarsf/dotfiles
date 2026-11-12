{
  self,
  withSystem,
  lib',
  ...
}:
{
  flake.nixosConfigurations.anuc = lib'.mkHost {
    inherit self withSystem;

    hostName = "anuc";
    stateVersion = "25.05";
    users = [ "runar" ];

    features = [
      "sshServer"
      "tmux"
      "docker"
      "teleport"
      {
        nginx = {
          domains = [ "runar.ch" ];
          email = "ssl@runar.ch";
          # Was commented out in the old config; the records are set by hand.
          dyndns = false;
        };
        containers = {
          user = "blahaj";
          # The login the services keep for themselves
          account = "runar";
        };
        navidrome.domain = "fm.runar.ch";
      }
      "slskd"
      "wrtag"
      "navidrome"
      "metadataRemote"
      "copyparty"
      "freshrss"
      "uptimeKuma"
      "flaresolverr"
      "grimmory"
      "karakeep"
      "octoFiesta"
    ];

    imports = [
      ./hardware-configuration.nix
      self.nixosModules.blahaj
    ];

    configuration = _: {
      host.desktop = false;

      # Served from this host but not managed from here.
      services.nginx.virtualHosts."nowplaying.runar.ch" = lib'.nginx.wildcardVhost "runar.ch" // {
        locations."/".proxyPass = "http://127.0.0.1:3067";
      };
    };
  };
}
