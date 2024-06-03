{ config, inputs, pkgs, keys ? [ ], ... }:

{
  # TODO Use system-wide sops module, look in nginx config
  imports = [ inputs.sops-nix.homeManagerModules.sops ];

  # home.activation.setupEtc = config.lib.dag.entryAfter [ "writeBoundary" ] ''
  #   /run/current-system/sw/bin/systemctl start --user sops-nix
  # '';

  # This converts your ssh key to an age key during every build,
  # puts it in $XDG_RUNTIME_DIR/secrets.d/age-keys.txt,
  # and points age.keyFile to the generated age key.
  sops.age.sshKeyPaths = keys;
  # Unfortunately, sops desn't look in the previously mentioned path for keys,
  # but luckily we can easily remedy this with a small wrapper script.
  home.packages = with pkgs; [ sops ];
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
