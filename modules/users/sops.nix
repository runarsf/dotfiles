{ inputs, pkgs, keys ? [ ], ... }:

{
  imports = [ inputs.sops-nix.homeManagerModules.sops ];

  home.packages = with pkgs; [ sops ];

  # This converts your ssh key to an age key during every build,
  # then point age.keyFile to the generated age key.
  sops.age.sshKeyPaths = keys;

  nixpkgs.overlays = [
    (_: prev: {
      sops = prev.symlinkJoin {
        name = "sops";
        paths = [
          (prev.writeShellScriptBin "sops" ''
            SOPS_AGE_KEY_FILE="''${XDG_RUNTIME_DIR}/secrets.d/age-keys.txt" exec ${prev.sops}/bin/sops ''${@}
          '')
          prev.sops
        ];
      };
    })
  ];
}
