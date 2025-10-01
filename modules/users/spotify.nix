{
  config,
  pkgs,
  inputs,
  outputs,
  ...
}: let
  cfg = config.modules.spotify;
in
  {
    imports = [inputs.spicetify-nix.homeManagerModules.default];
  }
  // outputs.lib.mkDesktopModule config "spotify" {
    options' = {
      spicetify = outputs.lib.mkEnableOption "Enable Spicetify";
    };

    config = {
      home.packages = with pkgs; outputs.lib.optionals (!cfg.spicetify) [spotify];

      programs.spicetify = let
        spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
      in {
        enable = cfg.spicetify;
        enabledExtensions = with spicePkgs.extensions; [
          hidePodcasts
          fullAppDisplayMod
          songStats
          betterGenres
          oneko
          sectionMarker
          shuffle
          lastfm
        ];
        enabledCustomApps = with spicePkgs.apps; [newReleases];
        # https://github.com/Gerg-L/spicetify-nix/blob/master/docs/THEMES.md
        theme = outputs.lib.mkDefault spicePkgs.themes.lucid;
      };
    };
  }
