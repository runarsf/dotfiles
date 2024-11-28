{ config, pkgs, inputs, outputs, name, ... }:

# TODO Waiting for zen to be merged to nixpkgs https://github.com/NixOS/nixpkgs/issues/327982

let
  version = "1.0.1-a.22";
  sha256 = "sha256:065rl1fhg79bkj1qy960qcid7wr7vd7j3wsf7bbr69b4rgmqqv3z";

in outputs.lib.mkDesktopModule config "zen" {
  nixpkgs.overlays = [
    (_: prev: {
      zen-browser =
        inputs.zen-browser.packages."${pkgs.system}".default.overrideAttrs
        (oldAttrs: {
          inherit version;
          src = builtins.fetchTarball {
            inherit sha256;
            url =
              "https://github.com/zen-browser/desktop/releases/download/${version}/zen.linux-generic.tar.bz2";
          };
        });
    })
  ];

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

  home.packages = [ pkgs.zen-browser ];

  home.file.".zen/${name}/chrome/userChrome.css".text = ''
    /* Not compatible with 'Allow Toolbar Theming'.
     */

    :root {
      --zen-main-browser-background: transparent !important;
    }

    /*
    #navigator-toolbox[zen-has-hover=true] #TabsToolbar {
      background: var(--zen-colors-tertiary) !important;
    }
    */
  '';
}
