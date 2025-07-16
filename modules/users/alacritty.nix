{
  outputs,
  config,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "alacritty" {
  # https://alacritty.org/config-alacritty.html
  programs.alacritty = {
    enable = true;
    package = pkgs.unstable.alacritty;
    settings = {
      window.dynamic_padding = true;
      font = {
        size = 12;
        normal.family = "JetBrains Mono Nerd Font";
      };
      draw_bold_text_with_bright_colors = true;
      colors = {
        primary = {
          background = "#0A0E14";
          foreground = "#B3B1AD";
        };
        normal = {
          black = "#01060E";
          red = "#EA6C73";
          green = "#91B362";
          yellow = "#F9AF4F";
          blue = "#53BDFA";
          magenta = "#FAE994";
          cyan = "#90E1C6";
          white = "#C7C7C7";
        };
        bright = {
          black = "#686868";
          red = "#F07178";
          green = "#C2D94C";
          yellow = "#FFB454";
          blue = "#59C2FF";
          magenta = "#FFEE99";
          cyan = "#95E6CB";
          white = "#FFFFFF";
        };
      };
      osc52 = "CopyPaste";
      mouse.bindings = [
        {
          mouse = "Middle";
          action = "PasteSelection";
        }
        {
          mouse = "Right";
          action = "Copy";
        }
      ];
      # FIXME https://github.com/alacritty/alacritty/issues/746
      keyboard.bindings = [
        {
          key = "C";
          mods = "Alt";
          action = "Copy";
        }
        {
          key = "V";
          mods = "Alt";
          action = "Paste";
        }
        {
          key = "=";
          mods = "Control";
          action = "ResetFontSize";
        }
        # { key = " Home,                    chars: "\x1bOH",   mode: AppCursor   }
        # { key = " Home,                    chars: "\x1b[H",   mode: ~AppCursor  }
        # { key = " End,                     chars: "\x1bOF",   mode: AppCursor   }
        # { key = " End,                     chars: "\x1b[F",   mode: ~AppCursor  }
      ];
    };
  };
}
