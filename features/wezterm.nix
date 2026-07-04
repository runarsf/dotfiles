{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.wezterm = {pkgs, ...}: {
    hardware.graphics = {
      enable = true;
      extraPackages = [pkgs.egl-wayland];
    };
    environment.systemPackages = [pkgs.egl-wayland];
    nix.settings = rec {
      substituters = ["https://wezterm.cachix.org"];
      trusted-substituters = substituters;
      trusted-public-keys = ["wezterm.cachix.org-1:kAbhjYUC9qvblTE+s7S+kl5XM1zVa4skO+E/1IDWdH0="];
    };
  };

  flake.homeModules.wezterm = {
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkIf;
    inherit (pkgs.stdenv.hostPlatform) system;
  in
    mkIf pkgs.stdenv.isLinux {
      home.shellAliases.ssh = "TERM=xterm-256color ssh";

      programs.wezterm = {
        enable = true;
        package = self.packages.${system}.wezterm;
      };
    };

  perSystem = {pkgs, ...}: {
    packages.wezterm = inputs.wrapper-modules.wrappers.wezterm.wrap {
      inherit pkgs;

      "wezterm.lua".content = ''
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
            tabline_a = { { 'hostname', padding = { left = 1, right = 0 } } },
            tabline_b = {},
            tabline_c = { ' ' },
            tab_active = {
              { 'index', padding = { left = 1, right = 0 } },
              'cwd',
              zoomed,
            },
            tab_inactive = {
              { 'index', padding = { left = 1, right = 0 } },
              { 'cwd', padding = { left = 1, right = 0 } },
              ':',
              { 'process', padding = { left = 0, right = 1 } },
              zoomed,
            },
            tabline_x = { ' ' },
            tabline_y = { { 'datetime', padding = { left = 0, right = 1 } } },
            tabline_z = {},
          },
          extensions = {},
        })

        tabline.apply_to_config(config)

        config.animation_fps = 120
        config.max_fps = 120
        config.cursor_blink_ease_in = 'Constant'
        config.cursor_blink_ease_out = 'Constant'
        config.font = wezterm.font_with_fallback({
          {
            family = 'JetBrains Mono',
            weight = 'Medium',
            harfbuzz_features = { 'calt', 'clig', 'liga', 'ss20', 'cv02', 'cv03', 'cv04', 'cv05', 'cv06', 'cv07', 'cv11', 'cv14', 'cv15', 'cv16', 'cv17' },
          },
          'JetBrainsMono Nerd Font',
          'CaskaydiaCove NFM',
        })
        config.font_size = 14.0
        config.warn_about_missing_glyphs = false
        config.default_cursor_style = "SteadyBar"
        config.window_decorations = "NONE"
        config.window_padding = { left = 0, right = 0, top = 0, bottom = 0 }

        function is_dark()
          if wezterm.gui then
            return wezterm.gui.get_appearance():find("Dark")
          end
          return true
        end

        config.check_for_updates = false

        config.set_environment_variables = {
          TERMINFO_DIRS = '~/.nix-profile/share/terminfo',
          WSLENV = 'TERMINFO_DIRS',
        }
        config.term = 'wezterm'
        config.use_dead_keys = false

        config.unix_domains = {
          { name = 'scratchpad' },
        }
        config.launch_menu = {
          { args = { 'btop' } },
          { label = 'Scratchpad', domain = { DomainName = 'scratchpad' } },
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
          { key = 'z', mods = 'LEADER', action = wezterm.action.TogglePaneZoomState },
          { key = '|', mods = 'LEADER', action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' } },
          { key = '-', mods = 'LEADER', action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' } },
          { key = 'a', mods = 'LEADER', action = wezterm.action.AttachDomain 'unix' },
          { key = 'd', mods = 'LEADER', action = wezterm.action.DetachDomain('CurrentPaneDomain') },
          {
            key = 'r',
            mods = 'LEADER',
            action = wezterm.action.PromptInputLine {
              description = 'Enter new name for session',
              action = wezterm.action_callback(function(window, pane, line)
                if line then
                  wezterm.mux.rename_workspace(window:mux_window():get_workspace(), line)
                end
              end),
            },
          },
          { key = 'l', mods = 'LEADER', action = wezterm.action.ShowLauncherArgs { flags = 'WORKSPACES' } },
          { key = 'LeftArrow', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(-1) },
          { key = 'RightArrow', mods = 'CTRL|SHIFT', action = wezterm.action.ActivateTabRelative(1) },
          { key = 'c', mods = 'ALT', action = wezterm.action.CopyTo("Clipboard") },
          { key = 'v', mods = 'ALT', action = wezterm.action.PasteFrom("Clipboard") },
          { key = 'T', mods = 'CTRL|SHIFT', action = wezterm.action.SpawnCommandInNewTab { cwd = wezterm.home_dir } },
        }

        wezterm.plugin.require('https://github.com/mrjones2014/smart-splits.nvim').apply_to_config(config, {
          direction_keys = { 'LeftArrow', 'DownArrow', 'UpArrow', 'RightArrow' },
          modifiers = {
            move = 'SHIFT',
            resize = 'SHIFT|ALT',
          },
        })

        config.use_resize_increments = true

        return config
      '';
    };
  };
}
