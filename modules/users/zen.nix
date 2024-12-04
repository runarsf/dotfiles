{ config, pkgs, inputs, outputs, name, ... }:

# TODO Waiting for zen to be merged to nixpkgs https://github.com/NixOS/nixpkgs/issues/327982

let
  hashes = {
    "twilight" = "sha256:03jk6v5ax0x38s7l9ra3l2i7wqfzs4mcq673jp2d0wa8xsqw8z4h";
    "1.0.1-a.22" =
      "sha256:065rl1fhg79bkj1qy960qcid7wr7vd7j3wsf7bbr69b4rgmqqv3z";
  };

in outputs.lib.mkDesktopModule' config "zen" (with outputs.lib; {
  version = mkOption {
    type = types.str;
    default = "1.0.1-a.22";
  };
  sha256 = mkOption {
    type = types.str;
    default = if hashes ? "${config.modules.zen.version}" then
      hashes.${config.modules.zen.version}
    else
      outputs.lib.fakeSha256;
  };
}) {
  nixpkgs.overlays = [
    (_: prev: {
      zen-browser =
        inputs.zen-browser.packages."${pkgs.system}".default.overrideAttrs
        (oldAttrs: {
          version = config.modules.zen.version;
          src = builtins.fetchTarball {
            sha256 = config.modules.zen.sha256;
            url =
              "https://github.com/zen-browser/desktop/releases/download/${config.modules.zen.version}/zen.linux-generic.tar.bz2";
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

  # home.file.".zen/${name}/chrome/userChrome.css".text = ''
  #   :root {
  #     --zen-main-browser-background: transparent !important;
  #   }
  #
  #   .browserContainer {
  #     background: #FAFAFA50;
  #     /* background: var(--zen-colors-tertiary); */
  #   }
  #
  #   /* Opaque tab sidebar
  #   #navigator-toolbox[zen-has-hover=true] #TabsToolbar {
  #     background: var(--zen-colors-tertiary) !important;
  #   }
  #   */
  # '';
}
