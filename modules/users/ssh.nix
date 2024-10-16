{ config, outputs, name, ... }:

outputs.lib.mkModule' config "ssh" {
  publicKeys = outputs.lib.mkOption {
    default = [ ];
    type = outputs.lib.types.listOf outputs.lib.types.str;
    description = "List of public key strings";
  };
} {
  services.ssh-agent.enable = true;
  programs.ssh.addKeysToAgent = "yes";
  # TODO users.extraUsers.root.openssh.authorizedKeys.keys

  # NOTE https://github.com/nix-community/home-manager/issues/322#issuecomment-1856128020
  # TODO Put public keys in .ssh
  home.file = {
    ".ssh/config" = {
      target = ".ssh/config_source";
      onChange =
        "cat ~/.ssh/config_source > ~/.ssh/config && chmod 400 ~/.ssh/config";
    };
  };

  nixos = {
    programs.ssh.startAgent = true;
    users.users."${name}".openssh.authorizedKeys.keys =
      outputs.lib.mkIf (config.modules.ssh.publicKeys != [ ]) config.modules.ssh.publicKeys;
  };
}
