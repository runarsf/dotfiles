{ pkgs, config, outputs, ... }:

{
  imports = [ ../trayer.nix ];

  programs.eww = {
    enable = true;
    configDir = ./.;
    package = with pkgs;
      if (outputs.lib.isWayland config) then eww-wayland else eww;
  };
  # wayland.windowManager.hyprland.settings.exec = [ ./launch-eww.sh ];
  wayland.windowManager.hyprland.settings.exec = [ "${config.home.homeDirectory}/.config/eww/launch-eww.sh" ];
}
