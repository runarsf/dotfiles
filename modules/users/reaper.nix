{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "reaper" {
  home.packages = with pkgs; [reaper];
}
