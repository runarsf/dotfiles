{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "streaming" {
  home.packages = with pkgs; [
    obs-studio
  ];
}
