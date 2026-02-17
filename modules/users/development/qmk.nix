{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "qmk" {
  nixos = {
    hardware.keyboard = {
      # zsa.enable = true;
      qmk.enable = true;
    };
    services.udev.packages = with pkgs.master; [
      qmk-udev-rules
      qmk
      via
      vial
    ];
    environment.systemPackages = with pkgs.master; [
      qmk
      via
      vial
      python313Packages.appdirs
    ];
  };

  # modules.udev.extraRules = [./qmk.rules];
}
