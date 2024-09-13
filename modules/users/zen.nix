{ config, pkgs, inputs, outputs, ... }:

outputs.lib.mkDesktopModule config "zen" {
  # modules.flatpak.enable = true;

  # nixos.services.flatpak.packages = [ "io.github.zen_browser.zen" ];

  home.packages = [
    inputs.zen-browser.packages."${pkgs.system}".default
  ];
}
