{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "minecraft" {
  home.packages = with pkgs; [prismlauncher];
}
