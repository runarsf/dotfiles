{ pkgs, name, ... }:

{
  # TODO Make this a nixos module and just import as a nixos module instead?
  nixos = {
    programs.adb.enable = true;
    users.users."${name}".extraGroups = [ "adbusers" "plugdev" "kvm" ];
    services.udev.packages = with pkgs; [ android-udev-rules ];
    environment.systemPackages = with pkgs; [ android-tools ];
  };
}
