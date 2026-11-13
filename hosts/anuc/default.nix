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
      # "slskd"
      "wrtag"
      "navidrome"
      "metadataRemote"
      "copyparty"
      "freshrss"
      "uptimeKuma"
      # "flaresolverr"
      "grimmory"
      "karakeep"
      # "octoFiesta"
    ];

    imports = [
      ./hardware-configuration.nix
      self.nixosModules.blahaj
    ];

    configuration = _: {
      host.desktop = false;

      boot.kernelParams = [
        # The link to the 870 EVO drops under write bursts; this chipset
        # defaults to min_power_with_partial.
        "ahci.mobile_lpm_policy=1"
        # Reboot after a panic instead of sitting dead.
        "panic=10"
      ];

      # A hang here went unnoticed for 15 days once.
      systemd.watchdog = {
        runtimeTime = "30s";
        rebootTime = "5m";
      };

      # Appends to hardware-configuration.nix's options: a flaky ESP
      # shouldn't drop the whole boot into emergency mode.
      fileSystems."/boot".options = [ "nofail" ];

      services.smartd = {
        enable = true;
        notifications.wall.enable = true;
      };

      # Served from this host but not managed from here.
      services.nginx.virtualHosts."nowplaying.runar.ch" = lib'.nginx.wildcardVhost "runar.ch" // {
        locations."/".proxyPass = "http://127.0.0.1:3067";
      };
    };
  };
}
