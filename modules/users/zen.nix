{
  config,
  pkgs,
  inputs,
  outputs,
  name,
  ...
}:

let
  # https://github.com/nix-community/home-manager/blob/release-24.05/modules/programs/firefox.nix#L57-L61
  userPrefValue =
    pref:
    builtins.toJSON (
      if builtins.isBool pref || builtins.isInt pref || builtins.isString pref then
        pref
      else
        builtins.toJSON pref
    );

  # TODO Make these options for the module
  prefs = {
    # Zen
    "browser.tabs.groups.enabled" = true;
    "tab.groups.add-arrow" = true;
    "tab.groups.background" = true;
    "tab.groups.borders" = true;
    "zen.urlbar.replace-newtab" = false;

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

    "devtools.toolbox.selectedTool" = "webconsole";
    "devtools.debugger.remote-enabled" = true;
    "devtools.chrome.enabled" = true;

    "browser.toolbars.bookmarks.visibility" = "newtab";
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
  };
  userChrome = {
    transparency = ''
      :root {
        --zen-main-browser-background: transparent !important;
      }
      #tabbrowser-tabpanels .browserStack {
        background: var(--zen-colors-tertiary, var(--toolbar-bgcolor));
      }
      [zen-compact-mode="true"] #nav-bar {
        background: var(--zen-dialog-background) !important;
      }
    '';
    fixBookmarksBar = ''
      #zen-appcontent-navbar-container:not(:has(#PersonalToolbar[collapsed="false"])) {
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
    advancedTabGroups = builtins.readFile "${
      pkgs.fetchgit {
        url = "https://github.com/Anoms12/Advanced-Tab-Groups.git";
        rev = "0dea07986100b26d24f2004794f110404723ab58";
        sha256 = "sha256-Vs0MjUjJC6xh3hB+VGK9dKxD0CRipMN2VE0IBNbP84g=";
        sparseCheckout = [ "tab-group.css" ];
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

in
outputs.lib.mkDesktopModule config "zen" {
  nixpkgs.overlays = [
    (_: prev: {
      zen-browser = inputs.zen-browser.packages.${pkgs.system}.default;
      # zen-browser = inputs.zen-browser.packages.${pkgs.system}.default.overrideAttrs (oldAttrs: rec {
      #   version = "1.7.6b";
      #   src = pkgs.fetchurl {
      #     hash = "sha256-InhljDorCxmXD9OCagF2RUNU9Lq8hIhz6/TqR7TSZG4=";
      #     url = "https://github.com/zen-browser/desktop/releases/download/${version}/zen.linux-x86_64.tar.xz";
      #   };
      # });
    })
  ];

  home.packages = with pkgs; [ zen-browser ];

  home.file = {
    ".zen/${name}/chrome/userChrome.css".text = builtins.concatStringsSep "\n" (
      with userChrome;
      [
        transparency
        fixBookmarksBar
        advancedTabGroups
      ]
    );
    ".zen/${name}/chrome/userContent.css".text = builtins.concatStringsSep "\n" (
      with userContent;
      [
        fixWhiteFlash
      ]
    );
    ".zen/${name}/user.js".text = builtins.concatStringsSep "\n" (
      outputs.lib.mapAttrsToList (key: value: ''user_pref("${key}", ${userPrefValue value});'') prefs
    );
  };

  xdg.mimeApps = outputs.lib.mkIf (config.defaultBrowser == "zen") {
    enable = true;
    defaultApplications =
      let
        entries = [
          "${pkgs.zen-browser}/share/applications/zen.desktop"
          "zen.desktop"
        ];
      in
      {
        "default-web-browser" = entries;
        "text/html" = entries;
        "x-scheme-handler/http" = entries;
        "x-scheme-handler/https" = entries;
        "x-scheme-handler/about" = entries;
        "x-scheme-handler/unknown" = entries;
      };
  };
}
