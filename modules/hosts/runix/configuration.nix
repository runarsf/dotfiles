{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.runixConfiguration = {
    pkgs,
    lib,
    ...
  }: {
    imports = [
      self.nixosModules.runixHardware
      self.nixosModules.niri

      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480s
    ];

    system.stateVersion = "24.05";

    nix.settings.experimental-features = ["nix-command" "flakes"];

    environment.systemPackages = with pkgs; [
      firefox
      vim
    ];

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
  };
}
