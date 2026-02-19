{
  config,
  outputs,
  ...
}: {
  options = with outputs.lib; {
    isDesktop = mkOption {
      default = config.home.sessionVariables ? NIXOS_OZONE_WL;
      type = types.bool;
    };
    # defaultBrowser = mkOption {
    #   default =
    #     if (config.isDesktop)
    #     then (throw "defaultBrowser not set")
    #     else null;
    #   type = types.nullOr types.str;
    # };
    avatar = mkOption {
      type = types.path;
    };
    PATH = mkOption {
      default = [];
      type = types.listOf types.str;
    };
    # NOTE: You may have to run 'sudo udevadm control --reload-rules && sudo udevadm trigger'
    modules.udev.extraRules = mkOption {
      default = [];
      type = types.listOf <| types.either types.path types.str;
        apply = rules:
        with builtins; let
          paths = rules |> filter isPath |> map readFile;
          strings = rules |> filter isString;
        in
          [ paths strings ] |> outputs.lib.flatten |> builtins.concatStringsSep "\n";
    };
  };

  config = {
    home.sessionVariables.PATH = outputs.lib.concatStringsSep ":" (config.PATH ++ ["\${PATH}"]);
    # modules."${config.defaultBrowser}".enable = config.isDesktop && config.defaultBrowser != null;
    nixos.services.udev.extraRules = config.modules.udev.extraRules;
  };
}
