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
          # protontricks
          # winetricks
          # protonup-qt
          # protonup-ng
          mangohud
          r2modman
        ];
        # sessionVariables = {
        #   STEAM_EXTRA_COMPAT_TOOLS_PATHS = "${config.home.homeDirectory}/.steam/root/compatibilitytools.d";
        # };
      };
    };

  flake.nixosModules.steam = { pkgs, ... }: {
    programs = {
      steam = {
        enable = true;
        remotePlay.openFirewall = true;
        localNetworkGameTransfers.openFirewall = true;
        gamescopeSession.enable = false;
        # extraCompatPackages = with pkgs; [ proton-ge-bin ];
      };
      gamemode = {
        enable = true;
        enableRenice = true;
        settings = {
          general = {
            softrealtime = "auto";
            renice = 10;
          };
          # custom = {
          #   start = "${lib.getExe pkgs.libnotify "notify-send"} -a 'Gamemode' 'Optimizations activated'";
          #   end = "${lib.getExe pkgs.libnotify "notify-send"} -a 'Gamemode' 'Optimizations deactivated'";
          # };
        };
      };
      gamescope = {
        enable = true;
        capSysNice = true;
      };
    };
  };
}
