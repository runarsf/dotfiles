{
  config,
  pkgs,
  inputs,
  outputs,
  name,
  ...
}:
{
  imports = [inputs.arkenfox.hmModules.arkenfox];
}
// outputs.lib.mkDesktopModule config "firefox" {
  xdg.mimeApps = outputs.lib.mkIf (config.defaultBrowser == "firefox") {
    enable = true;
    defaultApplications = {
      "default-web-browser" = ["firefox.desktop"];
      "text/html" = ["firefox.desktop"];
      "x-scheme-handler/http" = ["firefox.desktop"];
      "x-scheme-handler/https" = ["firefox.desktop"];
      "x-scheme-handler/about" = ["firefox.desktop"];
      "x-scheme-handler/unknown" = ["firefox.desktop"];
    };
  };

  # TODO Link hover effects from Firefox-Alpha
  # https://github.com/Naezr/ShyFox
  # home.file.".mozilla/firefox/${name}/chrome/" = {
  #   source = ./styles;
  #   recursive = true;
  # };

  # home.packages = with pkgs; [ nur.repos.rycee.mozilla-addons-to-nix ];

  # $ mozilla-addons-to-nix addons.json addons.nix
  # nixpkgs.overlays = [
  #   (_: prev: {
  #     ff-addons = import ./addons.nix {
  #       inherit (prev) fetchurl stdenv;
  #       buildFirefoxXpiAddon = pkgs.nur.repos.rycee.firefox-addons.buildFirefoxXpiAddon;
  #       lib = outputs.lib;
  #     };
  #   })
  # ];

  # home.packages = with pkgs; [ inputs.firefox-nightly.packages.${pkgs.system}.firefox-nightly-bin ];

  programs.firefox = {
    enable = true;

    arkenfox = {
      enable = true;
      version = "119.0";
    };

    profiles."${name}" = {
      isDefault = true;
      id = 0;
      # https://www.reddit.com/r/FirefoxCSS/comments/1etangp/the_new_way_to_have_transparency_in_new_tab/
      userContent = ''
        :root {
          --in-content-page-background: #00000000 !important;
          --in-content-box-background: #00000088 !important;
        }
      '';
      userChrome = ''
        :root {
          --bg: #00000000;
          --tabpanel-background-color: #00000000 !important;
        }

        /*window transparency*/
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

        /*tab selection*/
        tab.tabbrowser-tab[pending="true"] {
          color: #FFFFFFcc !important;
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
      # userChrome = ''
      #   @import url("edge-frfox/chrome/userChrome.css");
      #   @import url("edge-frfox/chrome/userContent.css");
      #   @import url("EdgyArc-fr/chrome/custom.css");
      # '';
      # userChrome = ''
      #   @media {
      #     :root {
      #       --sidebar-width: 48px;
      #       --sidebar-hover-width: 225px;
      #       --sidebar-debounce-delay: 500ms;
      #     }

      #     [sidebarcommand*="tabcenter"] #sidebar-header > *,
      #     [sidebarcommand*="tabcenter"] #sidebar-header,
      #     [sidebarcommand*="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header > *,
      #     [sidebarcommand*="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header,
      #     [sidebarcommand*="_3c078156-979c-498b-8990-85f7987dd929_-sidebar-action"] #sidebar-header > *,
      #     [sidebarcommand*="_3c078156-979c-498b-8990-85f7987dd929_-sidebar-action"] #sidebar-header {
      #       visibility: collapse;
      #       display: none;
      #     }

      #     #sidebar-box {
      #       background-color: transparent !important;
      #     }

      #     #sidebar-box[sidebarcommand*="tabcenter"],
      #     #sidebar-box[sidebarcommand*="treestyletab_piro_sakura_ne_jp-sidebar-action"],
      #     #sidebar-box[sidebarcommand*="_3c078156-979c-498b-8990-85f7987dd929_-sidebar-action"] {
      #       position: relative;
      #       min-width: var(--sidebar-width) !important;
      #       width: var(--sidebar-width) !important;
      #       max-width: var(--sidebar-width) !important;
      #       z-index: 1;
      #       /*display: grid !important;
      #       min-width: var(--sidebar-width) !important;
      #       max-width: var(--sidebar-width) !important;
      #       overflow: visible !important;
      #       height: 100% !important;
      #       min-height: 100% !important;
      #       max-height: 100% !important;*/
      #     }

      #     #sidebar-box[sidebarcommand*="tabcenter"] > #sidebar-splitter,
      #     #sidebar-box[sidebarcommand*="treestyletab_piro_sakura_ne_jp-sidebar-action"] > #sidebar-splitter,
      #     #sidebar-box[sidebarcommand*="_3c078156-979c-498b-8990-85f7987dd929_-sidebar-action"] > #sidebar-splitter {
      #       display: none !important;
      #     }

      #     #sidebar-box[sidebarcommand*="tabcenter"] > #sidebar,
      #     #sidebar-box[sidebarcommand*="treestyletab_piro_sakura_ne_jp-sidebar-action"] > #sidebar,
      #     #sidebar-box[sidebarcommand*="_3c078156-979c-498b-8990-85f7987dd929_-sidebar-action"] > #sidebar {
      #       transition: min-width 115ms linear var(--sidebar-debounce-delay) !important;
      #       min-width: var(--sidebar-width) !important;
      #       will-change: min-width;
      #       /*height: 100% !important;
      #       width: var(--sidebar-hover-width) !important;
      #       z-index: 200 !important;
      #       position: absolute !important;
      #       transition: width 150ms var(--sidebar-debounce-delay) ease !important;
      #       min-width: 0 !important;*/
      #     }

      #     #sidebar-box[sidebarcommand*="tabcenter"]:hover > #sidebar,
      #     #sidebar-box[sidebarcommand*="treestyletab_piro_sakura_ne_jp-sidebar-action"]:hover > #sidebar,
      #     #sidebar-box[sidebarcommand*="_3c078156-979c-498b-8990-85f7987dd929_-sidebar-action"]:hover > #sidebar {
      #       min-width: var(--sidebar-hover-width) !important;
      #       transition-delay: 100ms !important;
      #     }

      #     #main-window[sizemode="screen"] #sidebar-box,
      #     #main-window[sizemode="fullscreen"] #sidebar-box { --sidebar-width: 1px; }
      #     /* Move statuspanel to the other side when sidebar is hovered so it doesn't get covered by sidebar */
      #     #sidebar-box:not([positionend]):hover ~ #appcontent #statuspanel { inset-inline: auto 0px !important; }
      #     #sidebar-box:not([positionend]):hover ~ #appcontent #statuspanel-label { margin-inline: 0px !important; border-left-style: solid !important; }
      #   } /* }}} */

      #   @media {
      #   .toolbarbutton-icon,
      #   .unified-extensions-item-icon {
      #       filter: grayscale();
      #     }

      #     /* Hide UI buttons */
      #     #nav-bar:not([customizing="true"]) #back-button[disabled="true"],
      #     #nav-bar:not([customizing="true"]) #forward-button[disabled="true"],
      #     #nav-bar:not([customizing="true"]) #reload-button[disabled="true"],
      #     .urlbarView-row[type="tip"], /* remove "refresh firefox" button */
      #     .panel-subview-body toolbarseparator:not(:first-of-type),
      #     #appMenu-new-tab-button2,
      #     #appMenu-new-window-button2,
      #     #appMenu-new-private-window-button2,
      #     #appMenu-history-button,
      #     #appMenu-passwords-button,
      #     #appMenu-print-button2,
      #     #appMenu-save-file-button2,
      #     #appMenu-find-button2,
      #     #appMenu-quit-button2,
      #     #appMenu-zoom-controls2,
      #     #appMenu-downloads-button,
      #     #toolbar-context-menu menuseparator,
      #     #toggle_toolbar-menubar,
      #     #toolbar-context-selectAllTabs,
      #     #toolbar-context-undoCloseTab,
      #     #toggle_PersonalToolbar,
      #     #toolbar-context-bookmarkSelectedTab,
      #     #context_undoCloseTab,
      #     #context_closeTabOptions,
      #     #context_closeTab,
      #     #tabContextMenu menuseparator:not(:last-of-type),
      #     #context_openANewTab,
      #     #context_duplicateTab,
      #     #context_moveTabOptions,
      #     #share-tab-url-item,
      #     #context_selectAllTabs,
      #     #contentAreaContextMenu menuseparator:not(:last-of-type),
      #     #context-undo[disabled="true"],
      #     #context-redo[disabled="true"],
      #     #context-inspect-a11y,
      #     #context-take-screenshot,
      #     #context-savepage,
      #     #context-viewsource,
      #     #context-selectall,
      #     #context-sendpagetodevice,
      #     #sidebar-splitter,
      #     #context-back[disabled="true"],
      #     #context-forward[disabled="true"] {
      #       visibility: collapse;
      #       display: none;
      #     }
      #   }
      # '';

      # https://arkenfox.dwarfmaster.net/
      arkenfox = {
        enable = true;
        "0000".enable = true;
        "0100" = {
          enable = true;
          "0102"."browser.startup.page".value = 3;
        };
        "0200".enable = true;
        "0300".enable = true;
        "0600".enable = true;
        "0600"."0602".enable = false;
        "0800".enable = true;
        "0800" = {
          "0830".enable = false;
          "0803" = {
            "browser.search.suggest.enabled".value = true;
            "browser.urlbar.suggest.searches".value = true;
          };
        };
        "0900".enable = true;
        "1200".enable = true;
        "1700".enable = true;
        "2600" = {
          enable = true;
          "2608".enable = false;
          "2651".enable = false;
        };
      };
      settings = {
        "ui.osk.enabled" = true;
        "apz.overscroll.enabled" = false;
        "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;

        "browser.tabs.allow_transparent_browser" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "svg.context-properties.content.enabled" = true;
        "layout.css.color-mix.enabled" = true;
        "layout.css.light-dark.enabled" = true;
        "layout.css.backdrop-filter.enabled" = true;
        "layout.css.has-selector.enabled" = true;

        "devtools.toolbox.zoomValue" = 1.1;
        "devtools.toolbox.tabsOrder" = "inspector,webconsole,storage,netmonitor,styleeditor,jsdebugger,@react-devtools,@react-devtools";
        # "devtools.toolbox.host" = "bottom";
        "devtools.toolbox.selectedTool" = "webconsole";
        "devtools.cache.disabled" = true;
        "devtools.webconsole.input.editorOnboarding" = false;
        "devtools.screenshot.clipboard.enabled" = true;
        "devtools.screenshot.audio.enabled" = false;
        "devtools.responsive.touchSimulation.enabled" = true;
        "devtools.responsive.showUserAgentInput" = true;
        "devtools.netmonitor.persistlog" = true;
        "devtools.inspector.three-pane-enabled" = false;
        "devtools.inspector.activeSidebar" = "computedview";
        "devtools.dom.enabled" = false;
        "devtools.debugger.enabled" = false;
        "devtools.command-button-measure.enabled" = true;
        "devtools.application.enabled" = false;
        "devtools.accessibility.enabled" = false;
        "devtools.toolsidebar-width.inspector" = 0;
        "devtools.debugger.remote-enabled" = true;
        "devtools.chrome.enabled" = true;

        "browser.toolbars.bookmarks.visibility" = "newtab";
        "browser.translations.automaticallyPopup" = false;

        "layers.acceleration.force-enabled" = true;
        "gfx.webrender.all" = true;
        "browser.tabs.tabClipWidth" = 83;

        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.history" = true;
        "browser.urlbar.suggest.calculator" = true;
        "browser.urlbar.unitConversion.enabled" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.remotetab" = true;
        "browser.urlbar.suggest.topsites" = false;

        # "keyword.enabled" = true;
        # "browser.search.suggest.enabled" = false;
        # "browser.urlbar.suggest.searches" = false;

        "extensions.webextensions.tabhide.enabled" = true;

        "signon.rememberSignons" = false;
        "browser.rememberSignons" = false;

        "media.cache_readahead_limit" = 9999;
        "media.cache_resume_threshold" = 9999;

        "browser.tabs.warnOnClose" = false;
        "browser.tabs.warnOnCloseOtherTabs" = false;
        "browser.tabs.warnOnOpen" = false;

        "reader.parse-on-load.force-enabled" = true;
        "media.setsinkid.enabled" = true;

        "print.print_footerleft" = "";
        "print.print_footerright" = "";
        "print.print_headerleft" = "";
        "print.print_headerright" = "";

        "browser.disableResetPrompt" = true;
        "extensions.screenshots.disabled" = true;
        "extensions.pocket.enabled" = false;
        "ui.key.menuAccessKeyFocuses" = false;
        "media.block-autoplay-until-in-foreground" = false;
        "alerts.useSystemBackend" = true;

        # While this works, it messes with unstyled background colors :(
        # "browser.display.background_color" = "#2B2A33";
        # "browser.display.background_color.dark" = "#2B2A33";
        # "editor.background_color" = "#000000";

        "browser.tabs.drawInTitlebar" = true;
        "browser.tabs.inTitlebar" = 1;
        # "browser.uiCustomization.state" = outputs.lib.replaceStrings ["\n"] [""] (builtins.readFile ./uiState.json);

        # https://support.mozilla.org/en-US/kb/sync-custom-preferences
        "services.sync.prefs.dangerously_allow_arbitrary" = true;
        "services.sync.prefs.sync.browser.uiCustomization.state" = true;
        "services.sync.prefs.sync.browser.uidensity" = true;

        "mozilla.widget.use-argb-visuals" = true;

        "widget.gtk.rounded-bottom-corners.enabled" = true;

        "browser.aboutConfig.showWarning" = false;
      };
      # extensions = builtins.attrValues {
      #   inherit (pkgs.ff-addons)
      #     adaptive-tab-bar-colour
      #     dont-track-me-google1
      #     sidetabs
      #     smart-https-revived
      #     video-resumer;
      #   inherit (pkgs.nur.repos.bandithedoge.firefoxAddons)
      #     imagus;
      #   inherit (pkgs.nur.repos.rycee.firefox-addons)
      #     ublock-origin
      #     privacy-badger
      #     user-agent-string-switcher
      #     clearurls
      #     bypass-paywalls-clean
      #     smart-referer
      #     hacktools
      #     musescore-downloader
      #     fastforwardteam
      #     cookies-txt
      #     multi-account-containers facebook-container
      #     enhancer-for-youtube
      #     istilldontcareaboutcookies
      #     amp2html
      #     react-devtools
      #     localcdn
      #     return-youtube-dislikes
      #     duckduckgo-privacy-essentials
      #     gesturefy
      #     sponsorblock
      #     tabliss
      #     youtube-shorts-block
      #     temporary-containers
      #     video-downloadhelper
      #     auto-tab-discard
      #     bitwarden
      #     darkreader
      #     simple-tab-groups
      #     don-t-fuck-with-paste
      #     languagetool;
      # };
    };
  };
}
