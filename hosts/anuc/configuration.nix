{inputs, ...}: {
  imports = [
    ../../modules/linux/locales/norwegian.nix
    ../../modules/linux/systemd-boot.nix
    ../../modules/linux/network.nix
    ../../modules/linux/firewall.nix
    ../../modules/linux/openssh.nix
  ];

  system.stateVersion = "25.05";
}
