{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "qmk" {
  nixos = {
    hardware.keyboard = {
      zsa.enable = true;
      qmk.enable = true;
    };
    services.udev.packages = with pkgs; [
      qmk-udev-rules
      qmk
      via
      vial
    ];
    environment.systemPackages = with pkgs; [
      qmk
      via
      vial
      python313Packages.appdirs
    ];
  };

  # modules.udev.extraRules = [./qmk.rules];
}
