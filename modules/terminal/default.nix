{ lib', ... }:
let
  inherit (lib') mkOption types;

  terminalType = types.submodule {
    options = {
      package = mkOption {
        type = types.package;
      };

      class = mkOption {
        type = types.nullOr (types.functionTo (types.listOf types.str));
        default = null;
        description = ''
          Args making the terminal announce the given string as its app-id,
          or null when it can't.
        '';
      };
    };
  };
in
{
  flake.homeModules.terminal = lib'.mkModule "terminal" (
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib) getExe concatStringsSep;

      cfg = config.modules.terminal;

      terminal = cfg.terminals.${cfg.default};
      exe = getExe terminal.package;

      # `terminal [--class <id>] [args...]`, so window managers can spawn the
      # chosen terminal without knowing which one it is. --class is dropped
      # for terminals that can't set it.
      wrapper = pkgs.writeShellScriptBin "terminal" ''
        if [ "$1" = "--class" ]; then
          class="$2"
          shift 2
          exec ${exe} ${
            if terminal.class == null then "" else concatStringsSep " " (terminal.class "\"$class\"")
          } "$@"
        fi
        exec ${exe} "$@"
      '';
    in
    {
      options.modules.terminal = {
        terminals = mkOption {
          type = types.attrsOf terminalType;
          default = { };
          description = "Registered by each terminal's own module.";
        };

        default = mkOption {
          type = types.enum (builtins.attrNames cfg.terminals);
          default = "wezterm";
          description = "Which terminal window-manager binds spawn.";
        };

        command = mkOption {
          type = types.str;
          readOnly = true;
          default = getExe wrapper;
        };
      };

      config = {
        home.packages = [ wrapper ];
        home.sessionVariables.TERMINAL = "terminal";
      };
    }
  );
}
