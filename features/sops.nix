{inputs, ...}: {
  flake.nixosModules.sops = {
    config,
    lib,
    ...
  }: let
    cfg = config.features.sops;
  in {
    imports = [inputs.sops-nix.nixosModules.sops];

    options.features.sops = {
      vaultPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = inputs.vault;
        description = "Path to the secrets vault directory.";
      };
      ageKeyPaths = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = ["/etc/ssh/ssh_host_ed25519_key"];
        description = "SSH key paths to use as age keys for system sops.";
      };
    };

    config = lib.mkIf (cfg.vaultPath != null) {
      sops.age.sshKeyPaths = cfg.ageKeyPaths;
      sops.defaultSopsFile = lib.mkDefault "${cfg.vaultPath}/secrets.yaml";
    };
  };

  flake.homeModules.sops = {
    config,
    pkgs,
    lib,
    osConfig ? {},
    ...
  }: let
    cfg = config.features.sops;
    vaultPath = osConfig.features.sops.vaultPath or null;
  in {
    imports = [inputs.sops-nix.homeManagerModules.sops];

    options.features.sops = {
      ageKeyPaths = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = ["${config.home.homeDirectory}/.ssh/id_nix"];
        description = "SSH key paths to use as age keys for user sops.";
      };
    };

    config = lib.mkMerge [
      (lib.mkIf (vaultPath != null) {
        sops.age.sshKeyPaths = cfg.ageKeyPaths;
        sops.defaultSopsFile = lib.mkDefault "${vaultPath}/secrets.yaml";
      })
      {
        home.packages = [
          (pkgs.symlinkJoin {
            name = "sops";
            paths = [
              (pkgs.writeShellScriptBin "sops" ''
                SOPS_AGE_KEY_FILE="''${XDG_RUNTIME_DIR}/secrets.d/age-keys.txt" exec ${pkgs.sops}/bin/sops ''${@}
              '')
              pkgs.sops
            ];
          })
        ];
      }
    ];
  };
}
