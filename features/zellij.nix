_: {
  flake.homeModules.zellij =
    {
      lib,
      pkgs,
      config,
      ...
    }:
    let
      inherit (lib) getExe;

      zellij = getExe config.programs.zellij.finalPackage;
      zellijWrapped = pkgs.writeShellScriptBin "zellij" ''
        if [ "$#" -eq 0 ]; then
          "${zellij}" kill-session zj-launcher >/dev/null 2>&1
          "${zellij}" delete-session zj-launcher >/dev/null 2>&1

          export ZJ_FROM_LAUNCHER=1
          "${zellij}" --layout sm
          status=$?

          "${zellij}" kill-session zj-launcher >/dev/null 2>&1
          "${zellij}" delete-session zj-launcher >/dev/null 2>&1

          exit $status
        else
          exec "${zellij}" "$@"
        fi
      '';
    in
    {
      # TODO: overrides in zsh
      # if [[ -n "$ZELLIJ" && "$ZELLIJ_SESSION_NAME" == "zj-launcher" ]]; then
      # zellij kill-session "$ZELLIJ_SESSION_NAME"
      # fi
      programs.zellij = {
        enable = true;
        settings = {
          theme = "ayu_dark";
          theme_light = "ayu_light";
          session_serialization = false;
          show_startup_tips = false;
          # styled_underlines = false;
          # mouse_hover_effects = false;
          # mouse_hover_tips = false;
          # copy_on_select = false;
          ui = {
            pane_frames = {
              rounded_corners = true;
              hide_session_name = false;
            };
          };
          on_force_close = "quit";
          # simplified_ui = true;
          pane_frames = false;
          keybindings = {
            normal."bind \"Alt c\"" = [ "Copy" ];
          };
        };
        layouts.sm = {
          session_name = "zj-launcher";
          layout.pane.plugin = {
            location = "zellij:session-manager";
            is_welcome_screen = false;
          };
        };
      };

      # lib.hiPrio lowers the wrapper's nix-env priority number,
      # so when home-manager builds both the wrapper and the real zellij package try to place a bin/zellij,
      # the wrapper wins the collision instead of erroring out.
      home.packages = [ (lib.hiPrio zellijWrapped) ];
    };
}
