{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.sops = {
    config,
    lib,
    ...
  }: {
    imports = [inputs.sops-nix.nixosModules.sops];

    options = {
      features.sops = {
        vaultPath = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = "Path to the secrets vault directory.";
        };
        ageKeyPaths = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = ["/etc/ssh/ssh_host_ed25519_key"];
          description = "SSH key paths to use as age keys for system sops.";
        };
      };
    };

    config = lib.mkIf (config.features.sops.vaultPath != null) {
      sops.age.sshKeyPaths = config.features.sops.ageKeyPaths;
      sops.defaultSopsFile = lib.mkDefault "${config.features.sops.vaultPath}/secrets.yaml";
    };
  };

  flake.homeModules.sops = {
    config,
    lib,
    osConfig ? {},
    ...
  }: let
    vaultPath = osConfig.features.sops.vaultPath or null;
  in {
    imports = [inputs.sops-nix.homeManagerModules.sops];

    options = {
      features.sops.ageKeyPaths = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = ["${config.home.homeDirectory}/.ssh/id_nix"];
        description = "SSH key paths to use as age keys for user sops.";
      };
    };

    config = lib.mkIf (vaultPath != null) {
      sops.age.sshKeyPaths = config.features.sops.ageKeyPaths;
      sops.defaultSopsFile = lib.mkDefault "${vaultPath}/secrets.yaml";
    };
  };
}
