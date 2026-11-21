{ inputs, ... }:
{
  flake.homeModules.zen =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      inherit (builtins) readFile;
      inherit (pkgs) symlinkJoin fetchFromGitHub;
      inherit (pkgs.stdenv.hostPlatform) system;
      inherit (lib) mkEnableOption mkOption;
      inherit (lib.strings) concatStringsSep;
      inherit (lib.types) attrsOf anything;

      cfg = config.modules.zen;

      settings = {
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
      }
      // cfg.customPrefs;

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
          fetchFromGitHub {
            # follow:branch main
            owner = "Anoms12";
            repo = "Advanced-Tab-Groups";
            rev = "2136f303375588ecb6585f59a054109c8f1cde5d";
            hash = "sha256-8AOik681SkdNYWVjtFIsv4sQiinIdo14Qt0lbZf0yGQ=";
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

      legacyRoot = "${config.home.homeDirectory}/.zen";
      profileDir = "${config.xdg.configHome}/zen/default";

      # Zen's "Profile Groups" feature tracks the last-used profile in its own
      # sqlite state and ignores the classic profiles.ini Default=1 flag, so we
      # force the desired profile via --profile instead, wrapped into the bin
      # itself so it applies no matter how zen is launched (terminal, app
      # launcher, xdg-open default browser, etc).
      # zen-browser = symlinkJoin {
      #   name = "zen-browser";
      #   paths = [
      #     inputs.zen-browser.packages.${system}.default
      #   ];
      #   nativeBuildInputs = [ pkgs.makeWrapper ];
      #   postBuild = ''
      #     wrapProgram $out/bin/zen \
      #       --add-flags "--profile ${homeDirectory}/.zen/${username}"
      #   '';
      # };
    in
    {
      imports = [
        inputs.zen-browser.homeModules.beta
      ];

      options.modules.zen = {
        transparency = mkEnableOption "Enable transparency in Zen";
        customPrefs = mkOption {
          type = attrsOf anything;
          default = { };
          description = ''
            User-specific preferences to extend or
            replace defaults defined in this module
          '';
        };
      };

      config = {
        # Gecko only looks at $XDG_CONFIG_HOME/zen when ~/.zen is absent, so the
        # legacy root has to be moved out of the way, not just emptied.
        modules.migrations.entries.zen-profile = {
          when = ''[ -d "${legacyRoot}" ]'';
          locks = [
            "${legacyRoot}/*/.parentlock"
            "${profileDir}/.parentlock"
          ];
          before = [ "zen-browser-default" ];
          script = ''
            src=$(${lib.getExe pkgs.gawk} -v root="${legacyRoot}" -F= '
              /^\[Profile/ { n++; idx = n; inprof = 1; next }
              /^\[/        { inprof = 0; next }
              inprof && $1 == "Path"       { p[idx] = $2 }
              inprof && $1 == "IsRelative" { r[idx] = $2 }
              inprof && $1 == "Default" && $2 == "1" { d = idx }
              END {
                i = d ? d : (n == 1 ? 1 : 0)
                if (i && p[i] != "") print (r[i] == "0" ? p[i] : root "/" p[i])
              }
            ' "${legacyRoot}/profiles.ini")

            if [ -z "$src" ]; then
              echo "zen: no default profile in ${legacyRoot}/profiles.ini, migrate by hand"
            elif [ -e "${profileDir}/places.sqlite" ] &&
                 [ "$(readlink -f "$src")" != "$(readlink -f "${profileDir}")" ]; then
              echo "zen: ${profileDir} already holds another profile's data, migrate by hand"
            else
              hm-migrate merge "$src" "${profileDir}" --mode 700
              hm-migrate aside "${legacyRoot}"
            fi
          '';
        };

        programs.zen-browser = {
          enable = true;
          # package = zen-browser;
          setAsDefaultBrowser = true;
          policies = {
            DisableAppUpdate = true;
            DisableTelemetry = true;
            DisablePocket = true;
            DontCheckDefaultBrowser = true;
            DisableFeedbackCommands = true;
            DisableFirefoxStudies = true;
            NoDefaultBookmarks = true;
            OfferToSaveLogins = false;
            EnableTrackingProtection = {
              Value = true;
              Locked = true;
              Cryptomining = true;
              Fingerprinting = true;
            };
          };
          profiles.default = {
            inherit settings;

            userChrome =
              with userChrome;
              [
                fixBookmarksBar
              ]
              |> concatStringsSep "\n";
            userContent = with userContent; [ ] |> concatStringsSep "\n";
            presets.betterfox.enable = true;
            # pinsForce = true;
            # pins = {
            #   "Email" = {
            #     id = "1793453700828-5b4fec22-fa41-45b6-9db0-d50ed37a16e5";
            #     url = "https://mail.proton.me/";
            #     position = 101;
            #     isEssential = true;
            #   };
            #   "GitHub" = {
            #     id = "6a0ea64c-cfa8-49cc-9d97-97d669debfed";
            #     url = "https://github.com";
            #     position = 201;
            #     isEssential = true;
            #   };
            # };
            # spacesForce = true;
            # spaces = {
            #   "General" = {
            #     id = "748db55b-c584-4afc-85e1-ae6d7c089c2e";
            #     position = 1000;
            #     icon = "🏠";
            #   };
            #   "dev" = {
            #     id = "58d658f6-7b76-4a9e-b93e-c8d5a74bfa90";
            #     position = 2000;
            #   };
            # };
            search = {
              force = true;
              default = "kagi";
              engines = {
                kagi = {
                  name = "Kagi";
                  urls = [
                    {
                      template = "https://kagi.com/search?q={searchTerms}";
                    }
                  ];
                  definedAliases = [ "@k" ];
                };
              };
            };
            sine = {
              enable = true;
              mods = [
                "Nebula"
                "remove-tabs-separator"

                # Zen Mods
                "c01d3e22-1cee-45c1-a25e-53c0f180eea8" # Ghost Tabs
                "cb5efa80-f1e1-43ce-8c0b-fece8462d225" # Container Halo
              ];
            };
          };
        };

        # xdg.mimeApps = {
        #   enable = true;
        #   defaultApplications =
        #     let
        #       entries = [ "zen.desktop" ];
        #     in
        #     {
        #       "default-web-browser" = entries;
        #       "text/html" = entries;
        #       "x-scheme-handler/http" = entries;
        #       "x-scheme-handler/https" = entries;
        #       "x-scheme-handler/about" = entries;
        #       "x-scheme-handler/unknown" = entries;
        #     };
        # };
      };
    };

  flake.nixosModules.zen = _: {
    environment.etc = {
      "1password/custom_allowed_browsers" = {
        text = ''
          .zen-wrapped
        ''; # or just "zen" if you use unwrapped package
        mode = "0755";
      };
    };
  };
}
