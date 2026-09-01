{inputs, ...}: {
  libExtensions = [
    {
      mkUser = {
        self,
        username,
        homeModule,
        homeDirectory ? "/home/${username}",
        system ? "x86_64-linux",
      }:
        inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = import inputs.nixpkgs {inherit system;};
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
      }: {pkgs, ...}: let
        inherit (pkgs.stdenv.hostPlatform) system;
      in {
        imports = features ++ [self.nixosModules.primaryUser];
        primaryUsers = [username];
        nix.settings.trusted-users = [username];
        home-manager.users.${username} = self.homeModules.${username};
        users.users.${username} = {
          inherit initialPassword;
          isNormalUser = true;
          shell = self.packages.${system}.zsh;
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
  ];
}
