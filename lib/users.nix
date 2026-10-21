{
  inputs,
  lib,
  lib',
  ...
}:
let
  inherit (builtins) pathExists attrNames;

  inherit (lib) optionals catAttrs concatStringsSep;

  # Import every .nix file under <root>/_config, in the shape { homeModule?, nixosModule? }.
  # The dir is underscore-prefixed so import-tree doesn't import it.
  readUserConfig =
    self: root:
    let
      dir = root + "/_config";
      load =
        path:
        let
          entry = import path { inherit lib' inputs self; };
          unknown = attrNames (removeAttrs entry [ "homeModule" "nixosModule" ]);
        in
        if unknown == [ ] then
          entry
        else
          throw "mkUser: ${toString path} sets unknown attribute(s) ${concatStringsSep ", " unknown}; expected homeModule and/or nixosModule";
    in
    if root == null || !(pathExists dir) then [ ] else map load (lib'.concatPaths { paths = dir; });

  # Features every user gets unless they pass baseFeatures = false. Only ones
  # that take no per-user options: a feature someone configures belongs in that
  # user's own list, so each feature is named exactly once.
  defaultFeatures = [
    "cli"
    "discord"
    "flatpak"
    "fonts"
    "hypridle"
    "hyprlock"
    "hyprpaper"
    "network"
    "niks"
    "niri"
    "nix"
    "nixvim"
    "norwegian"
    "pipewire"
    "starship"
    "tuigreet"
    "vicinae"
    "wezterm"
    "xdg"
    "zed"
    "zsh"
  ];
in
{
  inherit defaultFeatures;

  # All three outputs for one user: the nixos module holding the account and
  # the nixos half of their features, the home module, and a standalone home
  # configuration. `features` is appended to defaultFeatures and resolved by
  # useFeatures; `home` is the user's own home-manager settings.
  mkUser =
    {
      self,
      withSystem,
      username,
      stateVersion,
      features ? [ ],
      baseFeatures ? true,
      home ? { },
      root ? null,
      extraGroups ? [ ],
      homeDirectory ? "/home/${username}",
      initialPassword ? "changeme",
      system ? "x86_64-linux",
    }:
    let
      resolved = lib'.useFeatures self (optionals baseFeatures defaultFeatures ++ features);
      userConfig = readUserConfig self root;
    in
    {
      flake.nixosModules.${username} = { pkgs, ... }: {
        imports = resolved.nixos ++ catAttrs "nixosModule" userConfig ++ [ self.nixosModules.primaryUser ];

        primaryUsers = [ username ];
        nix.settings.trusted-users = [ username ];

        home-manager.users.${username} = self.homeModules.${username};

        users.users.${username} = {
          inherit initialPassword;

          isNormalUser = true;
          # The interactive configuration lives in home-manager
          # (features/shells/zsh.nix); this only needs the plain shell binary.
          shell = pkgs.zsh;
          home = homeDirectory;
          extraGroups = [
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

      flake.homeModules.${username} = {
        imports = resolved.home ++ catAttrs "homeModule" userConfig ++ [ home ];

        home.stateVersion = stateVersion;
      };

      # pkgs comes from withSystem so standalone configs get the same package set
      # as the nixos ones, overlays and nixpkgs config included.
      flake.homeConfigurations.${username} = withSystem system (
        { pkgs, ... }:
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;

          extraSpecialArgs = { inherit self; };
          modules = [
            self.homeModules.${username}
            { home = { inherit username homeDirectory; }; }
          ];
        }
      );
    };
}
