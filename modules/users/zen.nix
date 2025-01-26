{ config, pkgs, inputs, outputs, name, ... }:

# about:config
#   browser.tabs.groups.enabled
#   tab.groups.add-arrow
#   tab.groups.background
#   tab.groups.borders

let
  zenStyles = {
    transparency = ''
      :root {
        --zen-main-browser-background: transparent !important;
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
    advancedTabGroups = builtins.readFile "${pkgs.fetchgit {
      url = "https://github.com/Anoms12/Advanced-Tab-Groups.git";
      rev = "0dea07986100b26d24f2004794f110404723ab58";
      sha256 = "sha256-Vs0MjUjJC6xh3hB+VGK9dKxD0CRipMN2VE0IBNbP84g=";
      sparseCheckout = [ "tab-group.css" ];
    }}/tab-group.css";
  };

in outputs.lib.mkDesktopModule config "zen" {
  nixpkgs.overlays = [
    (_: prev: {
      zen-browser = inputs.zen-browser.packages.${pkgs.system}.default;
    })
  ];

  home.packages = with pkgs; [ zen-browser ];

  home.file.".zen/${name}/chrome/userChrome.css".text =
    builtins.concatStringsSep "\n" (with zenStyles; [
      transparency
      fixBookmarksBar
      hideWorkspaces
      advancedTabGroups
    ]);

  xdg.mimeApps = outputs.lib.mkIf (config.defaultBrowser == "zen") {
    enable = true;
    defaultApplications = let
      entries =
        [ "${pkgs.zen-browser}/share/applications/zen.desktop" "zen.desktop" ];
    in {
      "default-web-browser" = entries;
      "text/html" = entries;
      "x-scheme-handler/http" = entries;
      "x-scheme-handler/https" = entries;
      "x-scheme-handler/about" = entries;
      "x-scheme-handler/unknown" = entries;
    };
  };
}
