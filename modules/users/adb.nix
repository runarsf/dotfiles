{ pkgs, name, ... }:

{
  nixos = {
    programs.adb.enable = true;
    users.users."${name}".extraGroups = [ "adbusers" ];
    services.udev.packages = [ pkgs.android-udev-rules ];
  };
}
