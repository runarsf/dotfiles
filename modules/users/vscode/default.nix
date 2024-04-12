{ config, pkgs, ... }: {
  programs.vscode = {
    enable = true;
    package = pkgs.unstable.vscode-fhs;
  };
}
