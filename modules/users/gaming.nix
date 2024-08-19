{
  config,
  pkgs,
  outputs,
  ...
}:

outputs.lib.mkDesktopModule config "gaming" {
  home.packages = with pkgs; [ prismlauncher ];

  nixos.services.ratbagd.enable = true;
}
