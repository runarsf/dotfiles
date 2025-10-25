{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkModule config "ssh" {
  options' = with outputs.lib; {
    keys = mkOption {
      default = [];
      type = types.attrsOf types.str;
      description = "List of public key strings";
    };
  };

  config = {
    # services.ssh-agent.enable = true;
    programs.ssh.addKeysToAgent = "yes";
    # TODO users.extraUsers.root.openssh.authorizedKeys.keys

    # NOTE https://github.com/nix-community/home-manager/issues/322#issuecomment-1856128020
    # File permissions: https://superuser.com/a/215506
    home.file =
      {
        ".ssh/config" = {
          target = ".ssh/config_source";
          onChange = "cat ~/.ssh/config_source > ~/.ssh/config && chmod 600 ~/.ssh/config";
        };
      }
      // builtins.listToAttrs (outputs.lib.mapAttrsToList (name: value: {
          name = ".ssh/${name}.pub";
          value = {
            text = value;
            onChange = "chmod 644 ~/.ssh/${name}.pub";
          };
        })
        config.modules.ssh.keys);

    nixos = {
      # programs.ssh.startAgent = true;
      users.users."${name}".openssh.authorizedKeys.keys =
        builtins.attrValues config.modules.ssh.keys;
    };
  };
}
