{
  self,
  inputs,
  lib',
  ...
}: let
  features = lib'.useFeatures self [
    "sops"
    "ssh"
    "fonts"
    "zsh"
    "starship"
    "hyprland"
    "hypridle"
    "hyprlock"
    "stylix"
    "dms"
    "wezterm"
    "niks"
    "nix"
    "vicinae"
    "disableLidSwitch"
    "network"
    "cli"
    "norwegian"
  ];
in {
  flake.nixosModules.runar = {
    pkgs,
    config,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    imports = features.nixos ++ [self.nixosModules.primaryUser];
    nix.settings.trusted-users = ["runar"];
    features = {
      ssh.keys = [
        {
          name = "id_priv";
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBT5zQFdVRooe5SfFZ2gKpruHF7FTw1OycTczRrLsR+M i@runar.ch";
        }
        {
          name = "id_nix";
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z nix";
        }
        {
          name = "id_ntnu";
          key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO6Y4kk5hFzs/B6vze9u9RPG9d+vVM5EIRIOug4OnJBk runarsfr@stud.ntnu.no";
        }
      ];
      stylix.wallpaper = ./outerwilds.jpeg;
    };
    home-manager.users.runar = self.homeModules.runar;
    users.users.runar = {
      openssh.authorizedKeys.keys = map (k: k.key) config.features.ssh.keys;
      isNormalUser = true;
      shell = self.packages.${system}.zsh;
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

    home.packages = with pkgs; [claude-code] ++ lib.optionals pkgs.stdenv.isLinux [television];

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
