{ pkgs, ... }:

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
      cursor  = "#C4C2BF";
      color0  = "#01060e";
      color8  = "#686868";
      color1  = "#ea6c73";
      color9  = "#f07178";
      color2  = "#91b362";
      color10 = "#c2d94c";
      color3  = "#f9af4f";
      color11 = "#ffb454";
      color4  = "#53bdfa";
      color12 = "#59c2ff";
      color5  = "#fae994";
      color13 = "#ffee99";
      color6  = "#90e1c6";
      color14 = "#95e6cb";
      color7  = "#c7c7c7";
      color15 = "#ffffff";
    };
  };

in {
  imports = [
    ./fonts.nix
  ];

  home.shellAliases.ssh = "TERM=xterm-256color ssh";

  # Bitmap fonts aren't—and will never be—supported by Kitty; this patch fixes that.
  # Considering there is no official support for them, some visdual bugs and crashes may occur.
  # You might have to change the cell size or line height to avoid crashes.
  #  $ kitty --debug-font-fallback -o=adjust_line_height=15 -o=font_family=...
  nixpkgs.overlays = [
    (_: prev: {
      patched-kitty = prev.kitty.overrideAttrs (previousAttrs: {
        patches = previousAttrs.patches ++ [
          ./kitty-bitmap.patch
        ];
      });
    })
  ];

  programs.kitty = {
    enable = true;
    package = pkgs.buildPackages.patched-kitty;

    font = {
      size = 12;
      # name = "JetBrainsMono Nerd Font";
      # package = pkgs.unstable.nerdfonts.override {
      #   fonts = [ "JetBrainsMono" ];
      # };

      name = "undefined medium";
      package = pkgs.undefined-medium;
    };

    keybindings = { };

    settings = {
      background_opacity = "0.85";
      background_blur = 40;

      cursor_shape = "block";
      cursor_underline_thickness = 1;
      cursor_stop_blinking_after = 0;
      remember_window_size = "no";

      modify_font = "underline_position 2";

      shell_integration = "no-cursor";
      hide_window_decorations = true;
      scrollback_lines = 10000;
      show_hyperlink_targets = true;
      enable_audio_bell = false;
      enabled_layouts = "tall,grid,fat,splits,stack";
      confirm_os_window_close = 0;
      allow_remote_control = true;
      tab_bar_min_tabs = 2;
      tab_bar_style = "separator";
      tab_separator = ''""'';
      tab_bar_edge = "bottom";
      cursor_blink_interval = 0;
      tab_title_template = ''"{fmt.bg._2D3036}{fmt.fg._0A0E14} {bell_symbol}{index} {fmt.bg.default}{fmt.fg.default} {title} "'';
      active_tab_title_template = ''"{fmt.bg._f9af4f}{fmt.fg._0A0E14} {bell_symbol}{index} {fmt.bg.default}{fmt.fg.default} {title} "'';
    } // themes."${theme}";
    extraConfig = let
      mod = "ctrl+shift";
      chord = "ctrl+a";
    in ''
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
      map f1 launch --stdin-source=@screen_scrollback --stdin-add-formatting --type=overlay less +G -R

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

      map alt+shift+left resize_window narrower
      map alt+shift+right resize_window wider
      map alt+shift+up resize_window taller
      map alt+shift+down resize_window shorter

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
