{ inputs, outputs, name, ... }:

{
  imports = [ inputs.arkenfox.hmModules.arkenfox ];

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

  programs.firefox = {
    enable = true;

    arkenfox = {
      enable = true;
      version = "119.0";
    };

    profiles."${name}" = {
      isDefault = true;
      id = 0;
      userChrome = builtins.concatStringsSep "\n" [
        # builtins.readFile ./userChrome.css
        ''
        #urlbar .urlbarView {
           display: none !important;
        }
        ''
      ];
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
        "0800"."0830".enable = false;
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

        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "svg.context-properties.content.enabled" = true;
        "layout.css.color-mix.enabled" = true;
        "layout.css.backdrop-filter.enabled" = true;

        "devtools.toolbox.zoomValue" = 1.1;
        "devtools.toolbox.tabsOrder" = "inspector,webconsole,storage,netmonitor,styleeditor,jsdebugger,@react-devtools,@react-devtools";
        "devtools.toolbox.host" = "bottom";
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

        "browser.urlbar.suggest.bestmatch" = true;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.history" = true;
        "browser.urlbar.suggest.calculator" = true;
        "browser.urlbar.unitConversion.enabled" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.remotetab" = false;
        "browser.urlbar.suggest.topsites" = false;

        "keyword.enabled" = true;
        "browser.search.suggest.enabled" = false;
        "browser.urlbar.suggest.searches" = false;

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

        "browser.display.background_color" = "#2B2A33";
        "browser.display.background_color.dark" = "#2B2A33";
        "editor.background_color" = "#000000";

        "browser.tabs.drawInTitlebar" = true;
        "browser.tabs.inTitlebar" = 1;
        # "browser.uiCustomization.state" = outputs.lib.replaceStrings ["\n"] [""] (builtins.readFile ./uiState.json);

        # https://support.mozilla.org/en-US/kb/sync-custom-preferences
        "services.sync.prefs.dangerously_allow_arbitrary" = true;
        "services.sync.prefs.sync.browser.uiCustomization.state" = true;
        "services.sync.prefs.sync.browser.uidensity" = true;
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
