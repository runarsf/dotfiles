{
  inputs,
  lib',
  ...
}: {
  flake.nixosModules.vicinae = _: {
    nix.settings = rec {
      substituters = ["https://vicinae.cachix.org"];
      trusted-substituters = substituters;
      trusted-public-keys = ["vicinae.cachix.org-1:1kDrfienkGHPYbkpNj1mWTr7Fm1+zcenzgTizIcI3oc="];
    };
  };

  flake.homeModules.vicinae = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib'.hyprland) lua onStart;
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    imports = [inputs.vicinae.homeManagerModules.default];

    programs.vicinae = {
      enable = true;
      systemd = {
        enable = true;
        autoStart = true;
        environment.USE_LAYER_SHELL = 1;
      };
      settings = {
        "$schema" = "https://vicinae.com/schemas/config.json";
        close_on_focus_loss = true;
        consider_preedit = true;
        pop_to_root_on_close = true;
        favicon_service = "twenty";
        search_files_in_root = true;
        providers."@sovereign/vicinae-extension-awww-switcher-0".preferences = {
          wallpaperPath = "${config.home.homeDirectory}/Pictures/Wallpapers";
          transitionDuration = 1;
        };
      };
      extensions = with inputs.vicinae-extensions.packages.${system}; [
        # bluetooth
        nix
        power-profile
        awww-switcher
        it-tools
        ssh
        color-converter
        niri # TODO: Only activate if niri is enabled
        wiktionary
        protondb-search
        zed-recents

        # Raycast extensions: https://github.com/raycast/extensions/tree/main/extensions
        (inputs.vicinae.lib.${system}.mkRayCastExtension {
          name = "karakeep"; # folder name in raycast/extensions repo
          rev = "c0c2cd66304c102d9e98d4d0d339f316d97ee41d";
          hash = "sha256-0zg7cbnlccWjOyL2Ulh5/gmRkQaHtYKcnwD7aBvMGS8=";
          # karakeep's build script uses `-o dist`, so output lands in dist/ not ~/.config/raycast/extensions/
          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp -r dist/* $out/
            runHook postInstall
          '';
        })
      ];
    };

    home.packages = with pkgs; [
      awww
      matugen
      sqlite # Needed for zed-recents
    ];

    wayland.windowManager.hyprland.settings = {
      bind = [
        {
          _args = [
            "SUPER + D"
            (lua "hl.dsp.exec_cmd(${builtins.toJSON "vicinae toggle"})")
            {}
          ];
        }
      ];
      layer_rule = [
        {
          match = {namespace = "vicinae";};
          blur = true;
        }
        {
          match = {namespace = "vicinae";};
          no_anim = true;
        }
      ];
      on = [
        (onStart (lib.getExe' pkgs.awww "awww-daemon"))
      ];
    };
  };
}
