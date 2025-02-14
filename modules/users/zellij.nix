{
  outputs,
  config,
  ...
}: let
  mkBinds = attrs:
    outputs.lib.listToAttrs (outputs.lib.mapAttrsToList (key: value: {
        name = ''bind "${key}"'';
        # value = "{ ${value}; }";
        value = [value];
      })
      attrs);
in
  outputs.lib.mkModule config "zellij" {
    programs.zellij = {
      enable = true;
      settings = {
        copy_on_select = false;
        ui = {
          pane_frames = {
            rounded_corners = true;
            hide_session_name = true;
          };
        };
        # keybindings = { normal = mkBinds { "Alt c" = "Copy"; }; };
        keybindings = {normal."bind \"Alt c\"" = ["Copy"];};
      };
    };
  }
