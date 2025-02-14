{
  outputs,
  config,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "zed" {
  home.packages = with pkgs.unstable; [
    # TODO Wait for zed support in home manager https://github.com/nix-community/home-manager/pull/5455
    nixd
    zed-editor
  ];
}
