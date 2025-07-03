{
  outputs,
  config,
  ...
}:
with outputs.lib; let
  cfg = config.programs.keychain;

  flags =
    cfg.extraFlags
    ++ optional (cfg.agents != [])
    "--agents ${concatStringsSep "," cfg.agents}"
    ++ optional (cfg.inheritType != null) "--inherit ${cfg.inheritType}";

  shellCommand = "${cfg.package}/bin/keychain --eval ${concatStringsSep " " flags} ${
    concatStringsSep " " cfg.keys
  }";
in
  outputs.lib.mkModule config "keychain" {
    programs.keychain = {
      enable = true;
      agents = ["ssh" "gpg"];
      enableZshIntegration = false;
      keys = config.modules.sops.privateKeyNames;
      # enableBashIntegration = false;
      # enableFishIntegration = false;
      # enableNushellIntegration = false;
      # enableZshIntegration = false;
    };

    wayland.windowManager.hyprland.settings.exec-once = [''eval "$(SHELL=bash ${shellCommand})"''];
  }
