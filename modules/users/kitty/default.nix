{ config, pkgs, lib, outputs, ... }:

let
  theme = "ayu-dark";
  themes = {
    ayu-dark = rec {
      background = "#0A0E14";
      foreground = "#B3B1AD";
      selection_background = "#C6C5C2";
      selection_foreground = "#2D3036";
      active_border_color = color3;
      inactive_border_color = background;
      cursor = "#C4C2BF";
      color0 = "#01060e";
      color8 = "#686868";
      color1 = "#ea6c73";
      color9 = "#f07178";
      color2 = "#91b362";
      color10 = "#c2d94c";
      color3 = "#f9af4f";
      color11 = "#ffb454";
      color4 = "#53bdfa";
      color12 = "#59c2ff";
      color5 = "#fae994";
      color13 = "#ffee99";
      color6 = "#90e1c6";
      color14 = "#95e6cb";
      color7 = "#c7c7c7";
      color15 = "#ffffff";
    };
  };

in outputs.lib.mkDesktopModule' config "kitty" {
  enableBitmap =
    outputs.lib.mkEnableOption "Enable bitmap fonts in Kitty";
  exe = outputs.lib.mkOption {
    default = outputs.lib.getExe config.programs.kitty.package;
    type = outputs.lib.types.str;
    readOnly = true;
  };
  exec = outputs.lib.mkOption {
    type = outputs.lib.types.functionTo outputs.lib.types.str;
    default = { class ? "scratchpad", command ? "" }:
      "${config.modules.kitty.exe} --class ${class} ${command}";
    readOnly = true;
  };
} {
  home.shellAliases.ssh = outputs.lib.mkIf (config.defaultTerminal == "kitty")
    "TERM=xterm-256color ssh";

  xdg.configFile."kitty/relative_resize.py" = {
    source = ./relative_resize.py;
    executable = true;
  };

  # Bitmap fonts aren't—and will never be—supported by Kitty; this patch fixes that.
  # Considering there is no official support for them, some visdual bugs and crashes may occur.
  # You might have to change the cell size or line height to avoid crashes.
  #  $ kitty --debug-font-fallback -o=adjust_line_height=15 -o=font_family=...
  nixpkgs.overlays = [
    (_: prev: {
      patched-kitty = prev.kitty.overrideAttrs (previousAttrs: {
        patches = previousAttrs.patches ++ [ ./bitmap-fonts.patch ];
      });
    })
  ];

  programs.kitty = {
    enable = true;
    package = with pkgs;
      if config.modules.kitty.enableBitmap then
        buildPackages.patched-kitty
      else
        kitty;

    keybindings = { };

    font.size = outputs.lib.mkDefault 14;

    settings = {
      # background_blur = 40;

      cursor_shape = "block";
      cursor_underline_thickness = 1;
      cursor_stop_blinking_after = 0;

      remember_window_size = "no";

      strip_trailing_spaces = "smart";

      modify_font = "underline_position 2";
      disable_ligatures = "always";

      shell_integration = "no-cursor";
      hide_window_decorations = true;
      scrollback_lines = 10000;
      enable_audio_bell = false;
      enabled_layouts = "tall,grid,fat,splits,stack";
      confirm_os_window_close = 0;
      allow_remote_control = true;
      tab_bar_min_tabs = 2;
      tab_bar_style = "separator";
      tab_separator = ''""'';
      tab_bar_edge = "bottom";
      cursor_blink_interval = 0;
      # tab_title_template = ''"{fmt.bg._2D3036}{fmt.fg._0A0E14} {bell_symbol}{index} {fmt.bg.default}{fmt.fg.default} {title} "'';
      # active_tab_title_template = ''"{fmt.bg._f9af4f}{fmt.fg._0A0E14} {bell_symbol}{index} {fmt.bg.default}{fmt.fg.default} {title} "'';
      tab_title_template = ''
        "{fmt.bg.default}{fmt.fg._2D3036}{fmt.bg._2D3036}{fmt.fg._0A0E14} {bell_symbol}{title} {fmt.bg.default}{fmt.fg._2D3036}{fmt.bg.default} "'';
      active_tab_title_template = ''
        "{fmt.bg.default}{fmt.fg._F9AF4F}{fmt.bg._F9AF4F}{fmt.fg._0A0E14} {bell_symbol}{title} {fmt.bg.default}{fmt.fg._F9AF4F}{fmt.bg.default} "'';
    } // themes."${theme}";
    extraConfig = let
      mod = "ctrl+shift";
      chord = "ctrl+a";
    in ''
      env PATH=${config.home.profileDirectory}/bin:''${PATH}

      map ${mod}+t new_tab
      map ${chord}>t new_tab
      map ${chord}>n new_tab
      map ${mod}+w close_tab
      map ${chord}>w close_tab
      map ${mod}+left previous_tab
      map ${mod}+right next_tab
      map ${mod}+page_up scroll_to_prompt -1
      map ${chord}>up scroll_to_prompt -1
      map ${mod}+page_down scroll_to_prompt 1
      map ${chord}>down scroll_to_prompt 1

      map ${mod}+space next_layout
      map ${chord}>space goto_layout stack
      map ${mod}+z toggle_layout stack
      map ${chord}>z toggle_layout stack

      map ctrl+plus change_font_size current +2
      map ctrl+minus change_font_size current -2

      map shift+up neighboring_window top
      map shift+down neighboring_window bottom
      map shift+left neighboring_window left
      map shift+right neighboring_window right

      map ctrl+return launch --cwd=current
      map ${chord}>return launch --cwd=current
      map ${mod}+return move_window_to_top
      map ${chord}>bar combine : goto_layout splits : launch --cwd=current --location=vsplit
      map ${chord}>minus combine : goto_layout splits : launch --cwd=current --location=hsplit

      # map alt+shift+left resize_window narrower
      # map alt+shift+right resize_window wider
      # map alt+shift+up resize_window taller
      # map alt+shift+down resize_window shorter

      map alt+shift+left kitten relative_resize.py left
      map alt+shift+down kitten relative_resize.py down
      map alt+shift+up kitten relative_resize.py up
      map alt+shift+right kitten relative_resize.py right

      map ${chord}>d detach_tab ask

      map alt+c copy_to_clipboard
      map alt+v paste_from_clipboard

      ${builtins.concatStringsSep "\n" (builtins.genList (x:
        let tab = let c = (x + 1) / 10; in builtins.toString (x + 1 - (c * 10));
        in ''
          map ${mod}+${tab} goto_tab ${toString (x + 1)}
          map ${chord}>${tab} goto_tab ${toString (x + 1)}
        '') 10)}
    '';
  };
}
