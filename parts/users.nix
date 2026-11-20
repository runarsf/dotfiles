{
  self,
  inputs,
  lib,
  lib',
  config,
  withSystem,
  ...
}:
let
  inherit (builtins) pathExists attrNames;
  inherit (lib)
    mkOption
    types
    mapAttrs
    optionals
    catAttrs
    concatStringsSep
    ;

  # Import every .nix file under <root>/_config, in the shape { homeModule?, nixosModule? }.
  # The dir is underscore-prefixed so import-tree doesn't import it.
  readUserConfig =
    root:
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
          throw "users: ${toString path} sets unknown attribute(s) ${concatStringsSep ", " unknown}; expected homeModule and/or nixosModule";
    in
    if root == null || !(pathExists dir) then [ ] else map load (lib'.concatPaths { paths = dir; });

  # Modules every user gets unless they set baseModules = false. Only ones
  # that take no per-user options: a module someone configures belongs in that
  # user's own list, so each module is named exactly once.
  defaultModules = [
    "cli"
    "discord"
    "fonts"
    "flatpak"
    "hypridle"
    "hyprlock"
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

  userModule =
    { name, ... }:
    {
      options = {
        stateVersion = mkOption { type = types.str; };
        modules = mkOption {
          type = lib'.modulesType;
          default = [ ];
          description = "Appended to the default modules.";
        };
        baseModules = mkOption {
          type = types.bool;
          default = true;
          description = "Whether to add the default modules.";
        };
        home = mkOption {
          type = types.deferredModule;
          default = { };
          description = "The user's own home-manager settings.";
        };
        root = mkOption {
          type = types.nullOr types.path;
          default = null;
          description = "Directory whose _config/ holds extra { homeModule?, nixosModule? } files.";
        };
        extraGroups = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };
        homeDirectory = mkOption {
          type = types.str;
          default = "/home/${name}";
        };
        initialPassword = mkOption {
          type = types.str;
          default = "changeme";
        };
        system = mkOption {
          type = types.str;
          default = "x86_64-linux";
          description = "System for the standalone home configuration.";
        };
      };
    };

  # An account for something the system runs rather than someone who logs in:
  # no home-manager, no modules, no wheel, no password. The uid is required
  # rather than allocated, since rootless podman and on-disk ownership both
  # depend on it staying put. Anything beyond the account itself is an
  # ordinary definition of users.users.<name> wherever it belongs.
  serviceUserModule =
    { name, config, ... }:
    {
      options = {
        uid = mkOption { type = types.int; };
        gid = mkOption {
          type = types.int;
          default = config.uid;
        };
        group = mkOption {
          type = types.str;
          default = name;
        };
        homeDirectory = mkOption {
          type = types.str;
          default = "/var/lib/${name}";
        };
        extraGroups = mkOption {
          type = types.listOf types.str;
          default = [ ];
        };
      };
    };

  resolve = user: lib'.useModules self (optionals user.baseModules defaultModules ++ user.modules);

  # The account and the nixos half of the user's modules.
  userNixosModule =
    username: user:
    { pkgs, ... }:
    {
      imports =
        (resolve user).nixos
        ++ catAttrs "nixosModule" (readUserConfig user.root)
        ++ [ self.nixosModules.primaryUser ];

      primaryUsers = [ username ];
      nix.settings.trusted-users = [ username ];

      home-manager.users.${username} = self.homeModules.${username};

      users.users.${username} = {
        inherit (user) initialPassword;

        isNormalUser = true;
        # The interactive configuration lives in home-manager
        # (modules/shells/zsh.nix); this only needs the plain shell binary.
        shell = pkgs.zsh;
        home = user.homeDirectory;
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
        ++ user.extraGroups;
      };
    };

  userHomeModule = _: user: {
    imports = (resolve user).home ++ catAttrs "homeModule" (readUserConfig user.root) ++ [ user.home ];

    home.stateVersion = user.stateVersion;
  };

  # pkgs comes from withSystem so standalone configs get the same package set
  # as the nixos ones, overlays and nixpkgs config included.
  userHomeConfiguration =
    username: user:
    withSystem user.system (
      { pkgs, ... }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        extraSpecialArgs = { inherit self; };
        modules = [
          self.homeModules.${username}
          {
            home = {
              inherit username;
              inherit (user) homeDirectory;
            };
          }
        ];
      }
    );

  serviceUserNixosModule =
    username: user:
    { pkgs, ... }:
    {
      users.users.${username} = {
        inherit (user) uid group extraGroups;

        isSystemUser = true;
        home = user.homeDirectory;
        createHome = true;
        shell = lib.mkDefault pkgs.bash;
      };

      users.groups.${user.group} = { inherit (user) gid; };
    };
in
{
  options = {
    users = mkOption {
      type = types.attrsOf (types.submodule userModule);
      default = { };
      description = "People who log in. Each gets nixosModules.<name>, homeModules.<name> and homeConfigurations.<name>.";
    };
    serviceUsers = mkOption {
      type = types.attrsOf (types.submodule serviceUserModule);
      default = { };
      description = "Accounts for services. Each gets nixosModules.<name>.";
    };
  };

  config.flake = {
    nixosModules = mapAttrs userNixosModule config.users // mapAttrs serviceUserNixosModule config.serviceUsers;
    homeModules = mapAttrs userHomeModule config.users;
    homeConfigurations = mapAttrs userHomeConfiguration config.users;
  };
}
