_: {
  flake.nixosModules.zed =
    { config, lib, ... }:
    {
      config = lib.mkIf (config.host.desktop or true) {
        nix.settings = rec {
          substituters = [ "https://zed.cachix.org" ];
          trusted-substituters = substituters;
          trusted-public-keys = [ "zed.cachix.org-1:/pHQ6dpMsAZk2DiP4WCL0p9YDNKWj2Q5FL20bNmw1cU=" ];
        };
      };
    };

  flake.homeModules.zed =
    {
      lib,
      osConfig,
      pkgs,
      ...
    }:
    {
      config = lib.mkIf (osConfig.host.desktop or true) {
        programs = {
          zed-editor = {
            enable = true;
            installRemoteServer = true;
          };
          television.enable = true;
        };

        home.packages = with pkgs; [
          ansifilter
        ];
      };
    };
}
