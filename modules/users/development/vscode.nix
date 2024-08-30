{
  config,
  outputs,
  pkgs,
  ...
}:

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
              "$@"
          '')
          prev.vscode-fhs
        ];
      };
    })
  ];
}
