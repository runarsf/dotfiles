{
  config,
  pkgs,
  inputs,
  outputs,
  ...
}:
{
  imports = [inputs.spicetify-nix.homeManagerModules.default];
}
// outputs.lib.mkDesktopModule config "spotify" {
  programs.spicetify = let
    spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
  in {
    enable = true;
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
    theme = spicePkgs.themes.lucid;
    colorScheme = "custom";
    customColorScheme = let
      c = hex: builtins.substring 1 (builtins.stringLength hex - 1) hex;
      bg = c "#0D1017";
      bg-alt = c "#131721";
      fg = c "#BFBDB6";
      secondary = c "#FFB454";
      secondary-alt = c "#E6B673";
    in {
      text = fg;
      subtext = fg;
      button-text = bg;
      subbutton-text = fg;
      main = bg;
      sidebar = bg;
      player = bg;
      card = bg-alt;
      shadow = bg;
      selected-row = bg-alt;
      sub-button = secondary;
      button = secondary;
      button-active = secondary-alt;
      button-disabled = bg-alt;
      sidebar-button = secondary;
      play-button = secondary;
      tab-active = bg;
      notification = bg;
      notification-error = bg;
      playback-bar = secondary;
      misc = secondary;
    };
  };
}
