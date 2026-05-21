{
  self,
  lib',
  ...
}: let
  features = lib'.useFeatures self [
    "sops"
    "hyprland"
    "hypridle"
    "hyprlock"
    "dms"
    "wezterm"
    "niks"
  ];
in {
  flake.nixosModules.runar = {pkgs, ...}: {
    imports = features.nixos;
    home-manager.users.runar = self.homeModules.runar;
    users.users.runar = {
      isNormalUser = true;
      shell = pkgs.bashInteractive;
    };
  };

  flake.homeModules.runar = {
    pkgs,
    lib,
    ...
  }: {
    imports = features.home;

    programs.bash.enable = true;
    programs.bash.shellAliases.ll = "ls -l";

    home.packages = with pkgs;
      [hello]
      ++ lib.optionals pkgs.stdenv.isLinux [television];

    home.stateVersion = "24.11";
  };
}
