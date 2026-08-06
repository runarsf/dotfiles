{ self, ... }: {
  flake.nixosModules.wine = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      wineWow64Packages.waylandFull
      winetricks
    ];
  };

  flake.homeModules.steam = { config, ... }: {
    home.sessionVariables = {
      STEAM_EXTRA_COMPAT_TOOLS_PATHS = "${config.home.homeDirectory}/.steam/root/compatibilitytools.d";
    };

    programs.lutris = {
      enable = true;
    };
  };

  flake.nixosModules.steam = { pkgs, ... }: {
    imports = [
      self.nixosModules.wine
    ];

    environment.systemPackages = with pkgs; [
      protontricks
      protonup-qt
      protonup-ng
      mangohud
      r2modman
    ];

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
