{
  config,
  outputs,
  ...
}: {
  options = with outputs.lib; {
    defaultTerminal = mkOption {
      type = types.nullOr types.str;
      default =
        if (config.isDesktop)
        then (throw "default terminal not set")
        else null;
    };
    modules.terminal = {
      exec = mkOption {
        type = types.functionTo types.str;
        readOnly = true;
        default = arg: let
          args =
            if !isList arg
            then [arg]
            else arg;
        in
          config.modules.terminal.exec'
          <| {
            command = args |> filter isString;
          }
          // (args
            |> filter isAttrs
            |> deepMerge);
      };
      exec' = mkOption {
        type = types.functionTo types.str;
        readOnly = true;
        default = config.modules."${config.defaultTerminal}".exec;
      };
    };
  };

  config = outputs.lib.mkIf (config.isDesktop && config.defaultTerminal != null) {
    home.sessionVariables.TERMINAL =
      config.modules."${config.defaultTerminal}".exec {}
      |> outputs.lib.splitString " "
      |> builtins.head
      |> builtins.baseNameOf;

    modules."${config.defaultTerminal}".enable = true;
  };
}
