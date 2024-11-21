{ config, pkgs, inputs, outputs, ... }:

{
  imports = [ inputs.spicetify-nix.homeManagerModules.default ];
} // outputs.lib.mkDesktopModule config "spotify" {
  programs.spicetify =
    let spicePkgs = inputs.spicetify-nix.legacyPackages.${pkgs.system};
    in {
      enable = true;
      enabledExtensions = with spicePkgs.extensions; [
        adblock
        hidePodcasts
        popupLyrics
        groupSession
        powerBar
        goToSong
        phraseToPlaylist
        songStats
        betterGenres
        #  playNext
        #  addToQueueTop
        oneko
        sectionMarker
        beautifulLyrics
        shuffle # shuffle+ (special characters are sanitized out of extension names)
      ];
      theme = spicePkgs.themes.text;
      enabledCustomApps = with spicePkgs.apps; [ newReleases ];
      #  colorScheme = "spotify";
    };

  /* home.packages = with pkgs; [
       (if (outputs.lib.isWayland config) then spotify-wayland else unstable.spotify)
     ];

     nixpkgs.overlays = [
       (_: prev: {
         spotify-wayland = prev.symlinkJoin {
           name = "spotify";
           paths = [
             (prev.writeShellScriptBin "spotify" ''
               exec ${pkgs.unstable.spotify}/bin/spotify \
                 --enable-features=UseOzonePlatform,WaylandWindowDecorations \
                 --ozone-platform-hint=auto \
                 --ozone-platform=wayland \
                 "$@"
             '')
             prev.spotify
           ];
         };
       })
     ];
  */
}
