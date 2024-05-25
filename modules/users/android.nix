{ pkgs, name, ... }:

{
  nixos = {
    programs.adb.enable = true;
    users.users."${name}".extraGroups = [ "adbusers" "plugdev" "kvm" ];
    services.udev.packages = with pkgs; [ android-udev-rules ];
    environment.systemPackages = with pkgs; [ android-tools ];
  };
}
