{ pkgs, ... }:

{
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true;
  };
  services.blueman.enable = true;
  environment.systemPackages = with pkgs; [
    blueberry
  ];
}
