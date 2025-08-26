{
  config,
  outputs,
  ...
}: let
  cfg = config.modules.ghostty;
in
  outputs.lib.mkDesktopModule config "ghostty" {
    options' = with outputs.lib; {
      # TODO: This one should be defined on modules.terminal.exec instead, and use modules.ghostty.exec'
      exec = mkOption {
        type = types.functionTo types.str;
        readOnly = true;
        default = arg: let
          args =
            if !isList arg
            then [arg]
            else arg;
        in
          cfg.exec'
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
        default = {
          command ? [],
          class ? null,
        }: let
          cmd = [(getExe config.programs.ghostty.package)] ++ optionals (length command > 0) (["-e"] ++ command);
        in
          concatStringsSep " " cmd;
      };
    };

    config = {
      programs.ghostty = {
        enable = true;
        settings = {
          font-family = [
            "ScientificaVector"
            "CozetteVector"
            "Unifont"
            "Unifont Upper"
            "CaskaydiaCove NFM"
          ];
          font-size = 16;
          theme = "dark:ayu,light:ayu_light";
          window-theme = "ghostty";
          cursor-style = "bar";
          cursor-style-blink = false;

          background-opacity = 0.75;

          gtk-adwaita = true;

          keybind = [
            "alt+c=copy_to_clipboard"
            "alt+v=paste_from_clipboard"
            "ctrl+a>|=new_split:right"
            "ctrl+a>-=new_split:down"
            "ctrl+a>z=toggle_split_zoom"
            "shift+left=goto_split:left"
            "shift+right=goto_split:right"
            "shift+up=goto_split:up"
            "shift+down=goto_split:down"
          ];
        };
      };
    };
  }
