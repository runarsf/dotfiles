{ config, pkgs, inputs, outputs, name, ... }:

outputs.lib.mkDesktopModule config "zen" {
  # modules.flatpak.enable = true;

  # nixos.services.flatpak.packages = [ "io.github.zen_browser.zen" ];

  xdg.mimeApps = outputs.lib.mkIf (config.defaultBrowser == "zen") {
    enable = true;
    defaultApplications = {
      "default-web-browser" = [ "zen.desktop" ];
      "text/html" = [ "zen.desktop" ];
      "x-scheme-handler/http" = [ "zen.desktop" ];
      "x-scheme-handler/https" = [ "zen.desktop" ];
      "x-scheme-handler/about" = [ "zen.desktop" ];
      "x-scheme-handler/unknown" = [ "zen.desktop" ];
    };
  };

  home.packages = [ inputs.zen-browser.packages."${pkgs.system}".default ];

  home.file.".zen/${name}/chrome/userChrome.css".text = ''
    :root {
      --bg: #00000000;
      --tabpanel-background-color: #00000000 !important;
      --zen-themed-toolbar-bg: #00000000 !important;
    }

    #main-window {
      background: var(--bg) !important;
    }

    /*current tab*/
    tab.tabbrowser-tab[selected="true"] stack.tab-stack vbox.tab-background {
      background: #FFFFFF22 !important;
    }

    /*hover tab*/
    tab.tabbrowser-tab:hover stack.tab-stack vbox.tab-background {
      background: #FFFFFF22 !important;
    }

    /*hibernated*/
    tab.tabbrowser-tab stack.tab-stack vbox.tab-background {
      background: transparent !important;
    }

    /*bookmarks*/
    toolbar {
      background: transparent !important;
    }

    /*idk*/
    #nav-bar {
      background: transparent  !important;
    }

    /*idk but keep*/
    #navigator-toolbox {
      background: transparent !important;
      border: none !important;
    }

    /*urlbar*/
    #urlbar-background {
      background: #00000044 !important;
    }

    /*suggestions dropdown*/
    #urlbar:is([open]) hbox#urlbar-background {
      background: #42414D !important;
    }

    /*little contextual buttons at left of urlbar*/
    #urlbar box#identity-box box {
      background: inherit !important;
    }
    #urlbar box#identity-box box:hover {
      background: #FFFFFF22 !important;
    }
    #urlbar box#identity-box box:active {
      background: #FFFFFF44 !important;
    }
  '';
}
