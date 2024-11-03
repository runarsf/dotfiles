{ config, pkgs, inputs, outputs, name, ... }:

# TODO https://github.com/NixOS/nixpkgs/issues/327982

let
  zenBrowserPkg = inputs.zen-browser.packages."${pkgs.system}".default;

  updatedZenBrowser = zenBrowserPkg.overrideAttrs (oldAttrs: rec {
    version = "1.0.1-a.17";
    src = builtins.fetchTarball {
      url =
        "https://github.com/zen-browser/desktop/releases/download/${version}/zen.linux-generic.tar.bz2";
      sha256 = "sha256:1n1cq0j8hifvwanqs3wsy5q69w04h397q09adxmdbydm6m8xn5k0";
    };
  });

in outputs.lib.mkDesktopModule config "zen" {
  # FIXME Use the desktop file from the program
  # xdg.mimeApps = outputs.lib.mkIf (config.defaultBrowser == "zen") {
  #   enable = true;
  #   defaultApplications = {
  #     "default-web-browser" = [ "zen.desktop" ];
  #     "text/html" = [ "zen.desktop" ];
  #     "x-scheme-handler/http" = [ "zen.desktop" ];
  #     "x-scheme-handler/https" = [ "zen.desktop" ];
  #     "x-scheme-handler/about" = [ "zen.desktop" ];
  #     "x-scheme-handler/unknown" = [ "zen.desktop" ];
  #   };
  # };

  home.packages = [ updatedZenBrowser ];

  home.file.".zen/${name}/chrome/userChrome.css".text = outputs.lib.trace "Zen: If using 'Bookmark Toolbar Tweaks', enable [uc.bookmarks.transparent]." ''
    /* Not compatible with 'Allow Toolbar Theming'.
     * If using 'Bookmark Toolbar Tweaks', enable [uc.bookmarks.transparent].
     */

    :root {
      --zen-main-browser-background: transparent !important;
    }
  '';
}
