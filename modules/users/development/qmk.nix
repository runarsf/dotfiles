{ config, pkgs, outputs, ... }:

outputs.lib.mkModule config "QMK" {
  home.packages = with pkgs.unstable; [
    qmk
  ];

  nixos.services.udev.packages =
    let
      qmk-rules = pkgs.writeTextFile {
        name = "50-qmk.rules";
        text = builtins.readFile ./qmk.rules;
        destination = "/etc/udev/rules.d/50-qmk.rules";
      };
    in [ qmk-rules ];
  nixos.services.udev.extraRules = builtins.readFile ./qmk.rules;
}