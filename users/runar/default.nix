{
  self,
  inputs,
  lib',
  ...
}: let
  features = lib'.useFeatures self [
    "sops"
    "fonts"
    "zsh"
    "starship"
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
      shell = pkgs.zsh;
      home = "/home/runar";
      initialPassword = "changeme";
      description = "Runar Fredagsvik";
      extraGroups = [
        "wheel"
        "networkmanager"
        "docker"
        "audio"
        "video"
        "libvirtd"
        "input"
        "i2c"
        "blahaj"
      ];
    };
  };

  flake.homeModules.runar = {
    pkgs,
    lib,
    ...
  }: {
    imports = features.home;

    home.packages = with pkgs; lib.optionals pkgs.stdenv.isLinux [television];

    home.stateVersion = "24.11";
  };

  flake.homeConfigurations.runar = inputs.home-manager.lib.homeManagerConfiguration {
    pkgs = import inputs.nixpkgs {system = "x86_64-linux";};
    extraSpecialArgs = {inherit self;};
    modules = [
      self.homeModules.runar
      {
        home = {
          username = "runar";
          homeDirectory = "/home/runar";
        };
      }
    ];
  };
}
