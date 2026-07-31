_: {
  flake.homeModules.steam =
    {
      config,
      pkgs,
      ...
    }:
    {
      home = {
        packages = with pkgs; [
          protontricks
          winetricks
          protonup-qt
          protonup-ng
          mangohud
          r2modman
        ];
        sessionVariables = {
          STEAM_EXTRA_COMPAT_TOOLS_PATHS = "${config.home.homeDirectory}/.steam/root/compatibilitytools.d";
        };
      };
    };

  flake.nixosModules.steam = _: {
    programs = {
      steam = {
        enable = true;
        remotePlay.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
        gamescopeSession.enable = true;
      };
      gamemode = {
        enable = true;
        enableRenice = true;
        settings = {
          general = {
            softrealtime = "auto";
            renice = 10;
          };
          custom = {
            start = "notify-send -a 'Gamemode' 'Optimizations activated'";
            end = "notify-send -a 'Gamemode' 'Optimizations deactivated'";
          };
        };
      };
      gamescope = {
        enable = true;
        capSysNice = true;
      };
    };
  };
}
