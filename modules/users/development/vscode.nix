{ config, outputs, pkgs, ... }:

outputs.lib.mkDesktopModule config "vscode" {
    programs.vscode = {
      enable = true;
      package = with pkgs; if (outputs.lib.isWayland config) then vscode-wayland else unstable.vscode-fhs;
    };

    nixpkgs.overlays = [
      (_: prev: {
        vscode-wayland = prev.symlinkJoin {
          name = "code";
          paths = [
            (prev.writeShellScriptBin "code" ''
              exec ${pkgs.unstable.vscode-fhs}/bin/code \
                --enable-features=UseOzonePlatform,WaylandWindowDecorations \
                --ozone-platform-hint=auto \
                --ozone-platform=wayland \
                "$@" # FIXME For some reason this doesn't work
            '')
            prev.vscode-fhs
          ];
        };
      })
      # (_: prev: {
      #   # TODO Make this into a generic nix function
      #   # TODO Chromium wayland
      #   vscode-wayland = prev.vscode-fhs.override {
      #     commandLineArgs = [
      #       "--enable-features=UseOzonePlatform,WaylandWindowDecorations"
      #       "--ozone-platform-hint=auto"
      #       "--ozone-platform=wayland"
      #       "--unity-launch"
      #     ];
      #   };
      # })
    ];
  }