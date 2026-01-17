{
  config,
  inputs,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "hytale" {
  home.packages = [inputs.hytale-launcher.packages.${pkgs.system}.default];
}
