{inputs, ...}: {
  mkUser = {
    self,
    username,
    homeModule,
    homeDirectory ? "/home/${username}",
    system ? "x86_64-linux",
  }:
    inputs.home-manager.lib.homeManagerConfiguration {
      # Matches the perSystem pkgs nixosConfigurations get via mkHost;
      # without it the standalone configs choke on e.g. zsh-abbr's license.
      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      extraSpecialArgs = {inherit self;};
      modules = [
        homeModule
        {
          home = {
            inherit username homeDirectory;
          };
        }
      ];
    };

  # Wraps the boilerplate common to every users/<name>/default.nix
  mkNixosUser = {
    self,
    username,
    features,
    extraGroups ? [],
    homeDirectory ? "/home/${username}",
    initialPassword ? "changeme",
  }: {pkgs, ...}: {
    imports = features ++ [self.nixosModules.primaryUser];
    primaryUsers = [username];
    nix.settings.trusted-users = [username];
    home-manager.users.${username} = self.homeModules.${username};
    users.users.${username} = {
      inherit initialPassword;
      isNormalUser = true;
      # The interactive configuration lives in home-manager
      # (features/shells/zsh.nix); this only needs the plain shell binary.
      shell = pkgs.zsh;
      home = homeDirectory;
      extraGroups =
        [
          "wheel"
          "networkmanager"
          "docker"
          "audio"
          "video"
          "libvirtd"
          "input"
          "i2c"
        ]
        ++ extraGroups;
    };
  };
}
