{
  config,
  pkgs,
  inputs,
  outputs,
  name,
  ...
}:
outputs.lib.mkDesktopModule config "eww" {
  # system = {
  #   boot.kernelModules = ["i2c-dev" "ddcci_backlight"];
  #   services.udev.extraRules = ''
  #         KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
  #   '';
  #   users.users."${name}".extraGroups = ["i2c"];
  # };

  home.packages = with pkgs; [
    ddcutil
    eww
  ];
  # programs.eww = {
  #   enable = true;
  #   configDir = ./.;
  #   package = pkgs.eww;
  # };
  # wayland.windowManager.hyprland.settings.exec = [ "${config.home.homeDirectory}/.config/eww/launch-eww.sh" ];
}
