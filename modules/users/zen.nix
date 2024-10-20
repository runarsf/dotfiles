{ config, pkgs, inputs, outputs, name, ... }:

let
  zenBrowserPkg = inputs.zen-browser.packages."${pkgs.system}".default;

  updatedZenBrowser = zenBrowserPkg.overrideAttrs (oldAttrs: rec {
    version = "1.0.1-a.11";
    src = builtins.fetchTarball {
      url =
        "https://github.com/zen-browser/desktop/releases/download/${version}/zen.linux-generic.tar.bz2";
      sha256 = "sha256:0bc7bwlf5r1zwfk0ynzrsv7b799s46rfdw95vmas19wigi7anr9l";
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

  home.file.".zen/${name}/chrome/userChrome.css".text = ''
    /* Not compatible with 'Allow Toolbar Theming'.
     * If using 'Bookmark Toolbar Tweaks', enable [uc.bookmarks.transparent].
     */

    :root {
      --zen-themed-toolbar-bg: transparent !important;
      /* --zen-colors-tertiary: transparent !important; */
    }

    .browserSidebarContainer {
      background: var(--zen-colors-tertiary) !important;
    }

    /*#main-window {
      background: var(--bg) !important;
    }*/

    /*current tab
    tab.tabbrowser-tab[selected="true"] stack.tab-stack vbox.tab-background {
      background: #FFFFFF22 !important;
    }*/

    /*hover tab
    tab.tabbrowser-tab:hover stack.tab-stack vbox.tab-background {
      background: #FFFFFF22 !important;
    }*/

    /*hibernated
    tab.tabbrowser-tab stack.tab-stack vbox.tab-background {
      background: transparent !important;
    }*/

    /*bookmarks
    toolbar:not(:hover) {
      background: transparent !important;
    }*/

    /*idk*/
    /*#nav-bar {
      background: transparent  !important;
    }*/

    /*idk but keep
    #navigator-toolbox {
      background: transparent !important;
      border: none !important;
    }*/

    /*urlbar*/
    #urlbar-background {
      background: #00000030 !important;
    }

    /* Suggestions dropdown */
    #urlbar:is([open]) hbox#urlbar-background {
      background: var(--tabpanel-background-color) !important;
      border: 1px solid var(--sidebar-border-color) !important;
      border-radius: var(--zen-border-radius) !important;
    }

    /* Little contextual buttons at left of urlbar */
    #urlbar box#identity-box box,
    #urlbar box#identity-box box:hover,
    #urlbar box#identity-box box:active {
      opacity: 0.8;
    }
  '';
}
