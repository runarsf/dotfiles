{
  outputs,
  config,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "zed" {
  programs = {
    zed-editor.enable = true;
    television.enable = true;
  };

  home.packages = with pkgs; [
    ansifilter
  ];
}
