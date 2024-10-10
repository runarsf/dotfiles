{ outputs, config, inputs, pkgs, name, keys ? [ ], ... }:

# Use this module by including it in your home.nix,
# along with the following snippet, where id_nix is the path to your sops key
# (in ssh-format):
# { _module.args.keys = [ "${config.home.homeDirectory}/.ssh/id_nix" ]; }
# Note that the key should also have read access to your vault repository,
# which is added as a flake input.
# For sops to activate, you also need to add a secret.

# TODO Make keys an option instead
# TODO Optionally use system-wide sops module, look in nginx and stylix config
{
  # TODO Why can't this be set in lib/users.nix
  imports = [ inputs.sops-nix.homeManagerModules.sops ];
} // outputs.lib.mkModule config "sops" {
  # home.activation.setupEtc = config.lib.dag.entryAfter [ "writeBoundary" ] ''
  #   run /run/current-system/sw/bin/systemctl start --user sops-nix
  # '';

  # This is for compatibility with templates.
  # TODO https://github.com/Mic92/sops-nix/issues/423
  nixos.sops = {
    age.sshKeyPaths = keys;
    defaultSopsFile = "${inputs.vault}/${name}/secrets.yaml";
  };

  sops.defaultSopsFile = "${inputs.vault}/${name}/secrets.yaml";

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
