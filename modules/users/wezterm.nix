{
  config,
  pkgs,
  outputs,
  ...
}:
let
  cfg = config.modules.wezterm;
  toLua =
    x:
    if
      builtins.match ''
        .*
        .*'' x != null
    then
      outputs.lib.trim x
    else
      "'${x}'";
  toLuaFonts = fonts: ''
    wezterm.font_with_fallback({
      ${fonts |> map toLua |> builtins.concatStringsSep ",\n  "}
    })
  '';
      # ${builtins.concatStringsSep ",\n  " (map toLua fonts)}
in
outputs.lib.mkDesktopModule config "wezterm" {
  options' = with outputs.lib; {
    package = mkOption {
      type = types.package;
      default = pkgs.unstable.wezterm; # inputs.wezterm.packages.${pkgs.stdenv.hostPlatform.system}.default;
    };
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
    # TODO Make a generic way that to connect the terminal to socket,
    # shouldn't crash if the exec' function doesn't support it.
    # Maybe exec'.extraOptions.socket=unix
    exec' = mkOption {
      type = types.functionTo types.str;
      default =
        {
          class ? null,
          command ? [ ],
        }:
        let
          cmd = [
            "start"
            "--always-new-process"
            "--"
          ] ++ command;
          args =
            if class == null then
              "${builtins.concatStringsSep " " cmd}"
            else
              "${builtins.head cmd} --class=${class} ${builtins.concatStringsSep " " (builtins.tail cmd)}";
        in
        "${getExe config.programs.wezterm.package} ${args}";
      readOnly = true;
    };
    # TODO Does this belong under somewhere else?
    bitmap = mkOption {
      type = types.bool;
      default = false;
    };
    fonts = mkOption {
      type = types.listOf types.str;
      default =
        let
          bitmap = [
            # TODO Use fonts from packages directly
            "scientifica"
            "CozetteHiDpi"
            "TamzenForPowerline"
            "Unifont"
            "Unifont Upper"
          ];
          vector = [
            ''
              {
                family = 'JetBrains Mono',
                weight = 'Medium',
                harfbuzz_features = { 'calt', 'clig', 'liga', 'ss20', 'cv02', 'cv03', 'cv04', 'cv05', 'cv06', 'cv07', 'cv11', 'cv14', 'cv15', 'cv16', 'cv17' },
              }
            ''
            "JetBrainsMono Nerd Font"
          ];
        in
        (if cfg.bitmap then bitmap else vector) ++ [ "CaskaydiaCove NFM" ];
    };
  };

  config = rec {
    home.shellAliases.ssh = outputs.lib.mkIf (
      config.defaultTerminal == "wezterm"
    ) "TERM=xterm-256color ssh";

    programs.nushell.shellAliases = {
      inherit (home.shellAliases) ssh;
    };

    nixos = {
      environment.systemPackages = with pkgs; [ egl-wayland ];
      hardware.graphics = {
        enable = true;
        extraPackages = with pkgs; [ egl-wayland ];
      };
    };

    home.packages = [ cfg.package ];
    programs.wezterm = {
      enable = true;
      package = cfg.package;
      # https://wezfurlong.org/wezterm/config/lua/config/index.html
      # FIXME Pane navigation doesn't work correctly
      extraConfig =
        let
          tab = ''
            { 'index', padding = { left = 1, right = 0, }, },
            'process',
            zoomed,
          '';
        in
        # lua
        ''
          local wezterm = require 'wezterm'
          local tabline = wezterm.plugin.require("https://github.com/michaelbrusegard/tabline.wez")
          local config = wezterm.config_builder()

          local function zoomed(tab)
            if tab.active_pane.is_zoomed then
              return wezterm.nerdfonts.md_fullscreen_exit .. ' '
            end
            return ""
          end

          tabline.setup({
            options = {
              icons_enabled = false,
              theme = 'Ayu Dark (Gogh)',
              section_separators = {
                left = wezterm.nerdfonts.ple_right_half_circle_thick,
                right = wezterm.nerdfonts.ple_left_half_circle_thick,
              },
              component_separators = {
                left = wezterm.nerdfonts.ple_right_half_circle_thin,
                right = wezterm.nerdfonts.ple_left_half_circle_thin,
              },
              tab_separators = {
                left = wezterm.nerdfonts.ple_right_half_circle_thick,
                right = wezterm.nerdfonts.ple_left_half_circle_thick,
              },
            },
            sections = {
              tabline_a = { 'hostname' },
              tabline_b = { },
              tabline_c = { ' ' },
              tab_active = {
                ${tab}
              },
              tab_inactive = {
                ${tab}
              },
              tabline_x = { 'battery' },
              tabline_y = { 'datetime' },
              tabline_z = { },
            },
            extensions = { },
          })

          tabline.apply_to_config(config)

          config.animation_fps = 120
          config.max_fps = 120
          config.cursor_blink_ease_in = 'Constant'
          config.cursor_blink_ease_out = 'Constant'
          config.font = ${toLuaFonts cfg.fonts}
          config.font_size = 14.0
          config.warn_about_missing_glyphs = false
          config.default_cursor_style = "SteadyBar"
          config.window_decorations = "NONE"
          config.use_resize_increments = true
          config.window_padding = {
            left = 0,
            right = 0,
            top = 0,
            bottom = 0,
          }

          -- config.tab_bar_at_bottom = true
          -- config.hide_tab_bar_if_only_one_tab = true
          -- config.use_fancy_tab_bar = false
          -- config.show_new_tab_button_in_tab_bar = false
          -- config.alternate_buffer_wheel_scroll_speed = 1

          -- Returns a bool based on whether the host operating system's
          -- appearance is light or dark.
          function is_dark()
            -- wezterm.gui is not always available, depending on what
            -- environment wezterm is operating in. Just return true
            -- if it's not defined.
            if wezterm.gui then
              -- Some systems report appearance like "Dark High Contrast"
              -- so let's just look for the string "Dark" and if we find
              -- it assume appearance is dark.
              return wezterm.gui.get_appearance():find("Dark")
            end
            return true
          end

          -- if is_dark() then
          --  config.color_scheme = 'Ayu Dark (Gogh)'
          -- else
          --  config.color_scheme = 'Ayu Light (Gogh)'
          -- end

          config.check_for_updates = false

          config.set_environment_variables = {
            TERMINFO_DIRS = '${config.home.profileDirectory}/share/terminfo',
            WSLENV = 'TERMINFO_DIRS',
          }
          config.term = 'wezterm'
          config.use_dead_keys = false

          config.unix_domains = {
            {
              name = 'scratchpad',
            },
          }
          config.launch_menu = {
            {
              args = { 'btop', },
            },
            {
              label = 'Scratchpad',
              domain = { DomainName = 'scratchpad', },
            },
          }

          config.tiling_desktop_environments = {
            'X11 LG3D',
            'X11 bspwm',
            'X11 i3',
            'X11 dwm',
            'Wayland',
          }

          config.leader = { key = 'a', mods = 'CTRL', timeout_milliseconds = 1000 }
          config.keys = {
            {
              key = 'z',
              mods = 'LEADER',
              action = wezterm.action.TogglePaneZoomState,
            },
            {
              key = '|',
              mods = 'LEADER',
              action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
            },
            {
              key = '-',
              mods = 'LEADER',
              action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
            },
            {
              key = 'a',
              mods = 'LEADER',
              action = wezterm.action.AttachDomain 'unix',
            },
            {
              key = 'd',
              mods = 'LEADER',
              action = wezterm.action.DetachDomain('CurrentPaneDomain'),
            },
            {
              key = 'r',
              mods = 'LEADER',
              action = wezterm.action.PromptInputLine {
                description = 'Enter new name for session',
                action = wezterm.action_callback(
                  function(window, pane, line)
                    if line then
                      wezterm.mux.rename_workspace(
                        window:mux_window():get_workspace(),
                        line
                      )
                    end
                  end
                ),
              },
            },
            {
              key = 'l',
              mods = 'LEADER',
              action = wezterm.action.ShowLauncherArgs { flags = 'WORKSPACES' },
            },
            {
              key = 'LeftArrow',
              mods = 'CTRL|SHIFT',
              action = wezterm.action.ActivateTabRelative(-1),
            },
            {
              key = 'RightArrow',
              mods = 'CTRL|SHIFT',
              action = wezterm.action.ActivateTabRelative(1),
            },
            {
              key = 'c',
              mods = 'ALT',
              action = wezterm.action.CopyTo("Clipboard"),
            },
            {
              key = 'v',
              mods = 'ALT',
              action = wezterm.action.PasteFrom("Clipboard"),
            },
            {
              key = 'T',
              mods = 'CTRL|SHIFT',
              action = wezterm.action.SpawnCommandInNewTab { cwd = wezterm.home_dir },
            }
          }

          -- wezterm.plugin.require("https://gitlab.com/xarvex/presentation.wez").apply_to_config(config)
          --
          -- wezterm.on("xarvex.presentation.activate", function(window)
          --   local overrides = window:get_config_overrides() or {}
          --
          --   overrides.font = ''$ {toLuaFonts ([ "DepartureMono Nerd Font" ] ++ cfg.fonts)}
          --
          --   window:set_config_overrides(overrides)
          -- end)
          --
          -- wezterm.on("xarvex.presentation.deactivate", function(window)
          --   local overrides = window:get_config_overrides() or {}
          --
          --   overrides.font = nil
          --
          --   window:set_config_overrides(overrides)
          -- end)

          wezterm.plugin.require('https://github.com/mrjones2014/smart-splits.nvim').apply_to_config(config, {
            direction_keys = { 'LeftArrow', 'DownArrow', 'UpArrow', 'RightArrow' },
            modifiers = {
              move = 'SHIFT',
              resize = 'SHIFT|ALT',
            },
          })

          return config
        '';
    };
  };
}
