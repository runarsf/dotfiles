{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "ghostty" {
  options' = with outputs.lib; {
    exec = mkOption {
      type = types.functionTo types.str;
      default =
        cmd:
        let
          command = if builtins.isString cmd then outputs.lib.splitString " " cmd else cmd;
        in
        cfg.exec' {
          inherit command;
        };
    };
    exec' = mkOption {
      type = types.functionTo types.str;
      default =
        {
          class ? null,
          command ? [ ],
        }:
        let
          cmd = [ "-e" ] ++ command;
          args = "${builtins.concatStringsSep " " cmd}";
        in
        "${getExe config.programs.ghostty.package} ${args}";
      readOnly = true;
    };
  };

  config = {
    programs.ghostty = {
      enable = true;
      settings = {
        font-family = [
          "ScientificaVector"
          "CozetteVector"
          "TamzenForPowerline"
          "Unifont"
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
