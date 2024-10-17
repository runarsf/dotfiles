{ pkgs, name, outputs, config, ... }:

outputs.lib.mkModule config "android" {
  nixos = {
    programs.adb.enable = true;
    users.users."${name}".extraGroups = [ "adbusers" "plugdev" "kvm" ];
    services.udev.packages = with pkgs; [ android-udev-rules ];
    environment.systemPackages = with pkgs; [ android-tools ];
  };

  home.packages = with pkgs.unstable; [
    flutter
    graphite2
    gtk3
    android-tools
    scrcpy
  ];

  nixpkgs.config.android_sdk.accept_license = true;

  home.file.".local/bin/adbrute".source = let
    script = pkgs.writeShellApplication {
      name = "adbrute";
      runtimeInputs = with pkgs; [ android-tools inetutils gnused nmap ];
      text = builtins.readFile ./adbrute.sh;
    };
  in "${script}/bin/adbrute";
}
