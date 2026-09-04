{inputs, ...}: {
  flake.homeModules.zen = {
    config,
    pkgs,
    lib,
    ...
  }: let
    cfg = config.features.zen;
  in {
    options.features.zen = {
      transparency = lib.mkEnableOption "Enable transparency in Zen";
    };
    config = let
      # TODO Zen Mods
      inherit (builtins) toJSON isBool isInt isString concatStringsSep readFile;
      inherit (pkgs.stdenv.hostPlatform) system;
      zen-browser-unwrapped = inputs.zen-browser.packages.${system}.default;
      # Zen's "Profile Groups" feature tracks the last-used profile in its own
      # sqlite state and ignores the classic profiles.ini Default=1 flag, so we
      # force the desired profile via --profile instead, wrapped into the bin
      # itself so it applies no matter how zen is launched (terminal, app
      # launcher, xdg-open default browser, etc).
      zen-browser = pkgs.symlinkJoin {
        name = "zen-browser";
        paths = [zen-browser-unwrapped];
        nativeBuildInputs = [pkgs.makeWrapper];
        postBuild = ''
          wrapProgram $out/bin/zen \
            --add-flags "--profile ${config.home.homeDirectory}/.zen/${config.home.username}"
        '';
      };

      # https://github.com/nix-community/home-manager/blob/release-24.05/modules/programs/firefox.nix
      userPrefValue = pref:
        toJSON (
          if isBool pref || isInt pref || isString pref
          then pref
          else toJSON pref
        );

      prefs = {
        # Zen
        "browser.tabs.groups.enabled" = true;
        "tab.groups.add-arrow" = true;
        "tab.groups.background" = true;
        "tab.groups.borders" = true;
        "zen.urlbar.replace-newtab" = false;
        "zen.widget.linux.transparency" = cfg.transparency;

        # Zen Context Menu
        "uc.hidecontext.screenshot" = true;
        "uc.hidecontext.unloadactions" = true;
        "uc.hidecontext.selectalltabs" = true;
        "uc.hidecontext.image" = true;
        "uc.hidecontext.printselection" = true;
        "uc.hidecontext.search" = true;
        "uc.hidecontext.searchinpriv" = true;
        "uc.hidecontext.closetab" = true;
        "uc.hidecontext.movetaboptions" = true;
        "uc.hidecontext.newtab" = true;
        "uc.hidecontext.mutetab" = true;
        "uc.hidecontext.menubar" = true;

        # Private Mode Highlighting
        "uc.private-browsing-top-bar.highlighting-style" = "";

        # Better Find Bar
        "theme.better_find_bar.transparent_background" = true;

        # UI
        "ui.osk.enabled" = true;
        "apz.overscroll.enabled" = false;
        "browser.tabs.allow_transparent_browser" = true;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "svg.context-properties.content.enabled" = true;
        "layout.css.color-mix.enabled" = true;
        "layout.css.light-dark.enabled" = true;
        "layout.css.backdrop-filter.enabled" = true;
        "layout.css.has-selector.enabled" = true;
        "browser.tabs.hoverPreview.enabled" = true;
        "browser.tabs.hoverPreview.showThumbnails" = true;

        "devtools.toolbox.selectedTool" = "webconsole";
        "devtools.debugger.remote-enabled" = true;
        "devtools.chrome.enabled" = true;

        "browser.tabs.insertAfterCurrent" = true;
        "browser.tabs.insertAfterCurrentExceptPinned" = true;
        "browser.toolbars.bookmarks.visibility" = "always";
        "browser.translations.automaticallyPopup" = false;

        "layers.acceleration.force-enabled" = true;
        "gfx.webrender.all" = true;

        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.history" = true;
        "browser.urlbar.suggest.calculator" = true;
        "browser.urlbar.unitConversion.enabled" = true;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.openpage" = false;
        "browser.urlbar.suggest.remotetab" = true;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.autoFill.adaptiveHistory.enabled" = false;

        "browser.search.isUS" = false;

        "extensions.webextensions.tabhide.enabled" = true;
        "signon.rememberSignons" = false;
        "media.cache_readahead_limit" = 9999;
        "media.cache_resume_threshold" = 9999;

        "print.print_footerleft" = "";
        "print.print_footerright" = "";
        "print.print_headerleft" = "";
        "print.print_headerright" = "";

        # https://support.mozilla.org/en-US/kb/sync-custom-preferences
        "services.sync.prefs.dangerously_allow_arbitrary" = true;
        "services.sync.prefs.sync.browser.uiCustomization.state" = true;
        "widget.gtk.rounded-bottom-corners.enabled" = true;
        "browser.aboutConfig.showWarning" = false;
        "browser.translations.neverTranslateLanguages" = "nb,de";
      };
      userChrome = {
        # transparency = ''
        #   :root {
        #     --zen-themed-toolbar-bg-transparent: transparent !important;
        #   }
        #   #tabbrowser-tabpanels .browserStack {
        #     background: var(--zen-colors-tertiary, var(--toolbar-bgcolor));
        #   }
        # '';
        fixBookmarksBar = ''
          :root[zen-single-toolbar="true"]:not([customizing]) #zen-appcontent-navbar-container:not(:has(#PersonalToolbar[collapsed="false"])) {
            height: var(--zen-element-separation) !important;

            .titlebar-buttonbox-container {
              display: none !important;
            }
          }
        '';
        hideWorkspaces = ''
          #zen-current-workspace-indicator,
          #zen-workspaces-button {
            display: none !important;
          }
        '';
        advancedTabGroups = readFile "${
          pkgs.fetchFromGitHub {
            owner = "Anoms12";
            repo = "Advanced-Tab-Groups";
            rev = "0dea07986100b26d24f2004794f110404723ab58";
            hash = "sha256-Vs0MjUjJC6xh3hB+VGK9dKxD0CRipMN2VE0IBNbP84g=";
            sparseCheckout = ["tab-group.css"];
          }
        }/tab-group.css";
      };
      userContent = {
        fixWhiteFlash = ''
          @namespace url("http://www.w3.org/1999/xhtml");

          @-moz-document url("about:home"),
          url("about:blank"),
          url("about:newtab") {
            body {
              background-color: var(--zen-colors-tertiary, var(--toolbar-bgcolor)) !important;
            }
          }
        '';
      };
    in {
      home.packages = [zen-browser];

      home.file = {
        ".zen/${config.home.username}/chrome/userChrome.css".text = concatStringsSep "\n" (
          with userChrome; [
            # fixBookmarksBar
          ]
        );
        ".zen/${config.home.username}/chrome/userContent.css".text = concatStringsSep "\n" (
          with userContent; [
            # fixWhiteFlash
          ]
        );
        ".zen/${config.home.username}/user.js".text = concatStringsSep "\n" (
          lib.mapAttrsToList (key: value: ''user_pref("${key}", ${userPrefValue value});'') prefs
        );
      };

      xdg.mimeApps = {
        enable = true;
        defaultApplications = let
          entries = ["zen.desktop"];
        in {
          "default-web-browser" = entries;
          "text/html" = entries;
          "x-scheme-handler/http" = entries;
          "x-scheme-handler/https" = entries;
          "x-scheme-handler/about" = entries;
          "x-scheme-handler/unknown" = entries;
        };
      };
    };
  };
}
