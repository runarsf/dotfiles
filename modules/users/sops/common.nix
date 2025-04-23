{
  outputs,
  config,
  inputs,
  pkgs,
  name,
  ...
}:
# Use this module by including it in your home.nix,
# along with the following snippet, where id_nix is the path to your sops key
# (in ssh-format):
# { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; }
# Note that the key should also have read access to your vault repository,
# which is added as a flake input.
# For sops to activate, you also need to add a secret.
# TODO Optionally use system-wide sops module, look in nginx and stylix config
{
  # TODO Why can't this be set in lib/users.nix
  imports = [inputs.sops-nix.homeManagerModules.sops];
}
// outputs.lib.mkModule config "sops" {
  options' = with outputs.lib; {
    ageKeys = mkOption {
      default = [];
      type = types.listOf types.str;
      description = "List of age key paths";
    };
    privateKeys = mkOption {
      default = [];
      type = types.listOf types.str;
      description = "List of private key names";
    };
    privateKeyNames = mkOption {
      default = let
        ageKeyNames =
          config.modules.sops.ageKeys
          |> map (key: outputs.lib.splitString "/" key)
          |> map (key: outputs.lib.last key);
      in
        ageKeyNames ++ config.modules.sops.privateKeys;
      readOnly = true;
    };
  };

  config = {
    # home.activation.setupEtc = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    #   run /run/current-system/sw/bin/systemctl start --user sops-nix
    # '';

    # This is for compatibility with templates.
    # TODO https://github.com/Mic92/sops-nix/issues/423
    nixos.sops = {
      age.sshKeyPaths = config.modules.sops.ageKeys;
      defaultSopsFile = "${inputs.vault}/${name}/secrets.yaml";
    };

    sops.defaultSopsFile = "${inputs.vault}/${name}/secrets.yaml";

    # This converts your ssh key to an age key during every build,
    # puts it in $XDG_RUNTIME_DIR/secrets.d/age-keys.txt,
    # and points age.keyFile to the generated age key.
    sops.age.sshKeyPaths = config.modules.sops.ageKeys;
    # Unfortunately, sops desn't look in the previously mentioned path for keys,
    # but luckily we can easily remedy this with a small wrapper script.
    home.packages = with pkgs; [sops];
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
  };
}
