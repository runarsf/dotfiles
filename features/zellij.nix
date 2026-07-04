_: {
  flake.homeModules.zellij = {lib, ...}: let
    inherit (lib) listToAttrs mapAttrsToList;

    mkBinds = attrs:
      listToAttrs (mapAttrsToList (key: value: {
          name = ''bind "${key}"'';
          value = [value];
        })
        attrs);
  in {
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
        keybindings = {normal."bind \"Alt c\"" = ["Copy"];};
      };
    };
  };
}
