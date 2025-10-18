{
  outputs,
  config,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "zed" {
  programs = {
    zed-editor = {
      enable = true;
      package = pkgs.unstable.zed-editor;
    };
    television = {
      enable = true;
    };
  };
}
