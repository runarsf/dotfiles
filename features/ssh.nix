_: {
  flake.nixosModules.ssh = {
    config,
    lib,
    ...
  }: let
    cfg = config.features.ssh;
  in {
    options.features.ssh.keys = lib.mkOption {
      default = [];
      description = "Public SSH keys. Added as authorized keys for all primaryUsers. First key is the default signing key.";
      type = lib.types.listOf (lib.types.submodule {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            description = "Key name, used as the filename (~/.ssh/<name>.pub).";
          };
          key = lib.mkOption {
            type = lib.types.str;
            description = "Public key string.";
          };
        };
      });
    };

    config = lib.mkIf (cfg.keys != []) {
      users.users = lib.genAttrs config.primaryUsers (_: {
        openssh.authorizedKeys.keys = map (k: k.key) cfg.keys;
      });
    };
  };

  flake.homeModules.ssh = {
    lib,
    pkgs,
    config,
    osConfig ? {},
    ...
  }: let
    keys = osConfig.features.ssh.keys or [];
    cfg = config.features.ssh;
  in {
    options.features.ssh.agentKeys = lib.mkOption {
      default = map (k: k.name) keys;
      defaultText = lib.literalExpression "map (k: k.name) osConfig.features.ssh.keys";
      type = lib.types.listOf lib.types.str;
      description = ''
        Names of private keys (files at ~/.ssh/<name>, e.g. from
        features.sops.privateKeys) to load into the agent once per
        graphical login.
      '';
    };

    config = {
      programs.ssh = {
        enable = true;
        matchBlocks."*".addKeysToAgent = "yes";
      };

      # gcr-ssh-agent (started by features.tuigreet's PAM/gnome-keyring
      # integration) listens here; export it session-wide via
      # environment.d so it's set before hyprland/niri even start.
      systemd.user.sessionVariables.SSH_AUTH_SOCK = "%t/gcr/ssh";

      # Loads cfg.agentKeys into the agent once per graphical login.
      # gcr-ssh-agent pulls the passphrase from the (PAM-unlocked) keyring,
      # so this runs silently after the one-time "remember" prompt.
      systemd.user.services.ssh-add-agent-keys = lib.mkIf (cfg.agentKeys != []) {
        Unit = {
          Description = "Add SSH keys to the agent";
          PartOf = ["graphical-session.target"];
        };
        Service = let
          ssh-add = lib.getExe' pkgs.openssh "ssh-add";
          keyPaths = map (name: "${config.home.homeDirectory}/.ssh/${name}") cfg.agentKeys;
        in {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${lib.getExe pkgs.bash} -c '${ssh-add} -l >/dev/null 2>&1 || ${ssh-add} ${lib.concatStringsSep " " keyPaths}'";
        };
        Install.WantedBy = ["graphical-session.target"];
      };

      home.file = builtins.listToAttrs (map (k:
        lib.nameValuePair ".ssh/${k.name}.pub" {text = k.key;})
      keys);
    };
  };
}
