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
    # https://github.com/Gerg-L/spicetify-nix/blob/master/docs/THEMES.md
    theme = outputs.lib.mkDefault spicePkgs.themes.lucid;
  };
}
