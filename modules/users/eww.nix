{ pkgs, config, outputs, ... }:

{
  imports = [ ./trayer.nix ];

  programs.eww = {
    enable = true;
    configDir = ./.;
    package = if outputs.lib.isWaylans then pkgs.eww else pkgs.eww-wayland;
  };
  wayland.windowManager.hyprland.settings.exec = [ "${config.home.homeDirectory}/.config/eww/launch-eww.sh" ];
}
