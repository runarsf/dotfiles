{ inputs, ... }:
{
  flake.homeModules.zen =
    {
      lib,
      pkgs,
      config,
      osConfig,
      ...
    }:
    let
      inherit (lib)
        mkEnableOption
        mkOption
        mkIf
        ;
      inherit (lib) getAttrs attrValues;
      inherit (lib.strings) concatStringsSep;
      inherit (lib.types)
        attrsOf
        listOf
        anything
        str
        ;

      cfg = config.modules.zen;

      settings = {
        # Zen
        "browser.tabs.groups.enabled" = true;
        "tab.groups.add-arrow" = true;
        "tab.groups.background" = true;
        "tab.groups.borders" = true;
        "zen.urlbar.behavior" = "float";
        "zen.urlbar.replace-newtab" = false;
        "zen.widget.linux.transparency" = cfg.transparency;
        "zen.tabs.show-newtab-vertical" = false;
        "zen.pinned-tab-manager.restore-pinned-tabs-to-pinned-url" = true;
        "zen.window-sync.sync-only-pinned-tabs" = true;
        "zen.tabs.ctrl-tab.ignore-pending-tabs" = true;
        "zen.workspaces.continue-where-left-off" = true;
        "zen.glance.activation-method" = "ctrl";
        "zen.glance.animation-duration" = 150;
        "intl.regional_prefs.use_os_locales" = true;

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
        "browser.translations.neverTranslateLanguages" = "en,nb,de";
      }
      // {
        # Private Mode Highlighting
        "uc.private-browsing-top-bar.highlighting-style" = "";

        # Super Pins
        "uc.superpins.border.pins" = false;
        "browser.sessionstore.restore_pinned_tabs_on_demand" = true;
        "zen.workspaces.show-workspace-indicator" = false;
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
        userChromePresets = mkOption {
          type = listOf str;
          default = [ ];
        };
        userContentPresets = mkOption {
          type = listOf str;
          default = [ ];
        };
        mods = mkOption {
          type = listOf str;
          default = [
            "ff-better-findbar"
            "context-menu-icons"

            # Zen Mods
            "c01d3e22-1cee-45c1-a25e-53c0f180eea8" # Ghost Tabs
            "58649066-2b6f-4a5b-af6d-c3d21d16fc00" # Private Mode Highlighting
            "ad97bb70-0066-4e42-9b5f-173a5e42c6fc" # Super Pins
          ];
          description = ''
            Zen and Sine mods to be installed.
            Get the IDs from https://zen-browser.app/mods and https://github.com/sineorg/store/tree/main/mods.
          '';
        };
      };

      config = mkIf (osConfig.host.desktop or true) {
        modules.migrations.entries =
          let
            legacyRoot = "${config.home.homeDirectory}/.zen";
            profileDir = "${config.xdg.configHome}/zen/default";
            modsDir = "${profileDir}/chrome/sine-mods";
            modsFile = "${modsDir}/mods.json";
            zenModsFile = "${profileDir}/zen-themes.json";
            zenModsDir = "${profileDir}/chrome/zen-themes";

            find = lib.getExe' pkgs.findutils "find";
            jq = lib.getExe pkgs.jq;
            keep = lib.escapeShellArg (builtins.toJSON cfg.mods);
            stale = ''${jq} -r --argjson keep ${keep} '(keys - $keep)[]' "${modsFile}"'';
          in
          {
            # Gecko only looks at $XDG_CONFIG_HOME/zen when ~/.zen is absent, so the
            # legacy root has to be moved out of the way, not just emptied.
            zen-profile = {
              when = ''[ -d "${legacyRoot}" ]'';
              locks = [
                "${legacyRoot}/*/.parentlock"
                "${profileDir}/.parentlock"
              ];
              before = [ "zen-browser-default" ];
              script = ''
                # Any directory holding a profile, for a legacy root whose
                # profiles.ini is gone or never got written.
                found=$(${find} "${legacyRoot}" -maxdepth 3 -name places.sqlite -printf '%h\n' | sort -u)
                count=$(printf '%s\n' "$found" | ${lib.getExe pkgs.gawk} 'NF' | wc -l)

                src=""
                if [ -f "${legacyRoot}/profiles.ini" ]; then
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
                fi
                if [ -z "$src" ] && [ "$count" = 1 ]; then
                  src="$found"
                fi

                if [ -n "$src" ] && [ ! -d "$src" ]; then
                  echo "zen: ${legacyRoot} points at $src, which is not there, migrate by hand"
                elif [ -z "$src" ] && [ "$count" != 0 ]; then
                  echo "zen: cannot tell which of the $count profiles under ${legacyRoot} to take, migrate by hand"
                elif [ -z "$src" ]; then
                  echo "zen: no profile data in ${legacyRoot}, moving it aside so Gecko uses ${profileDir}"
                  hm-migrate aside "${legacyRoot}"
                elif [ -e "${profileDir}/places.sqlite" ] &&
                     [ "$(readlink -f "$src")" != "$(readlink -f "${profileDir}")" ]; then
                  echo "zen: ${profileDir} already holds another profile's data, migrate by hand"
                else
                  hm-migrate merge "$src" "${profileDir}" --mode 700
                  hm-migrate aside "${legacyRoot}"
                fi
              '';
            };

            # Sine copies zen-themes.json into mods.json on every start and only
            # deletes it once every mod folder has been copied, so one mod without
            # a folder makes that import repeat forever.
            zen-mods = {
              when = ''
                [ -e "${zenModsFile}" ] || [ -e "${zenModsDir}" ] ||
                  { [ -f "${modsFile}" ] && [ -n "$(${stale} 2>/dev/null || true)" ]; }
              '';
              locks = [ "${profileDir}/.parentlock" ];
              before = [ "zen-browser-default" ];
              after = [
                "writeBoundary"
                "migrate-zen-profile"
              ];
              script = ''
                for legacy in "${zenModsFile}" "${zenModsDir}"; do
                  if [ -e "$legacy" ]; then
                    hm-migrate aside "$legacy"
                  fi
                done

                if [ -f "${modsFile}" ]; then
                  ${stale} | while read -r id; do
                    echo "zen: dropping mod $id, it is not in modules.zen.mods"
                    run rm -rf "${modsDir}/$id"
                  done

                  if [ -n "''${DRY_RUN:-}" ]; then
                    echo "zen: would prune ${modsFile}"
                  else
                    ${jq} --argjson keep ${keep} \
                      'with_entries(select(.key as $k | $keep | index($k)))' \
                      "${modsFile}" > "${modsFile}.tmp"
                    mv "${modsFile}.tmp" "${modsFile}"
                  fi
                fi
              '';
            };
          };

        programs.zen-browser = {
          enable = true;
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

          nativeMessagingHosts = [
            pkgs.firefoxpwa
          ];
          env = {
            GTK_THEME = "Adwaita";
          };

          profiles.default = {
            inherit settings;

            presets.betterfox.enable = true;

            userChrome =
              userChrome
              |> getAttrs cfg.userChromePresets
              |> attrValues
              |> concatStringsSep "\n";
            userContent =
              userContent
              |> getAttrs cfg.userContentPresets
              |> attrValues
              |> concatStringsSep "\n";

            sine = {
              inherit (cfg) mods;

              enable = true;
            };

            keyboardShortcuts = [
              {
                id = "zen-compact-mode-toggle";
                key = "c";
                modifiers = {
                  control = true;
                  alt = true;
                };
              }
              {
                id = "key_savePage";
                key = "s";
                modifiers = {
                  control = true;
                };
              }
              {
                id = "zen-toggle-sidebar";
                key = "x";
                modifiers = {
                  control = true;
                  alt = true;
                };
              }
              {
                id = "key_quitApplication";
                disabled = true;
              }
              {
                id = "key_reload";
                key = "r";
                modifiers.control = true;
              }
              {
                id = "key_reload_skip_cache";
                key = "r";
                modifiers = {
                  control = true;
                  shift = true;
                };
              }
            ];
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
            # search = {
            #   force = true;
            #   default = "kagi";
            #   engines = {
            #     kagi = {
            #       name = "Kagi";
            #       urls = [
            #         {
            #           template = "https://kagi.com/search?q={searchTerms}";
            #         }
            #       ];
            #       definedAliases = [ "@k" ];
            #     };
            #   };
            # };
          };
        };
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
