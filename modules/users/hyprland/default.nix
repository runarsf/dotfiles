{
  config,
  inputs,
  outputs,
  pkgs,
  system,
  ...
} @ self:
# TODO temporary command (nix-shell) with fuzzel
# TODO Better switching of layouts (master, centered master, dwindle, pseudo) https://wiki.hyprland.org/Configuring/Master-Layout/#workspace-rules
# TODO See if smart_resizing negates the need for a resizing script
# TODO refactor master layout config
# TODO Make a fisdeurbel nushell script that re-runs the stream if it dies
let
  lock = "${pkgs.hyprlock}/bin/hyprlock";
  hypr-snap = pkgs.writers.writePython3 "hypr-snap" {
    flakeIgnore = [
      "E305"
      "E501"
      "E227"
      "E302"
      "E225"
    ];
  } (builtins.readFile ./bin/hypr-snap.py);
  hypr-gamemode = pkgs.writers.writePython3 "hypr-gamemode" {
    flakeIgnore = [
      "E305"
      "E501"
      "E227"
      "E302"
      "E225"
      "E731"
    ];
  } (builtins.readFile ./bin/hypr-gamemode.py);
  hypr-move = "${pkgs.writers.writeNuBin "hypr-move" (builtins.readFile ./bin/move.nu)}/bin/hypr-move";
  hypr-workspace = "${pkgs.writers.writeNuBin "hypr-workspace" (builtins.readFile ./bin/workspace.nu)}/bin/hypr-workspace";
  hypr-scratch-group = "${pkgs.writers.writeNuBin "hypr-scratch-group" (builtins.readFile ./bin/scratch-group.nu)}/bin/hypr-scratch-group";
in
  {
    imports = [
      # inputs.hyprland.nixosModules.default
      inputs.hyprland.homeManagerModules.default
      (
        import ./binds.nix
        (self // {inherit hypr-gamemode lock hypr-snap hypr-workspace hypr-move;})
      )
      ./rules.nix

      (
        import ./pyprland.nix (self // {inherit hypr-scratch-group;})
      )
      ./hypridle.nix
      ./hyprlock.nix
      ./hyprpaper.nix
      ./hyprpanel.nix
    ];
  }
  // outputs.lib.mkDesktopModule' config "hyprland"
  {
    animations = outputs.lib.mkEnableOption "Enable funky animations";
  }
  {
    modules = outputs.lib.enable [
      "wayland"
      "fuzzel"
      "hyprpanel"
      "hypridle"
      "hyprlock"
      "hyprpaper"
      "pyprland"
      # "ulauncher"
      # "waybar"
    ];
    services = outputs.lib.enable ["kanshi"]; # swaync
    # programs.jq.enable = true;

    home.packages = with pkgs; [
      nwg-displays
      nemo
      unstable.hyprsunset
      unstable.hyprpolkitagent
      wl-clipboard
      libsForQt5.qt5.qtwayland
    ];

    nixos = {
      programs = {
        xwayland.enable = true;
        uwsm.enable = true;

        hyprland = {
          enable = true;
          withUWSM = true;
          package = inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
          portalPackage =
            inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
        };
      };
      hardware.graphics = {
        enable = true;
        package =
          inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.mesa.drivers;

        enable32Bit = true;
        package32 =
          inputs.hyprland.inputs.nixpkgs.legacyPackages.${pkgs.stdenv.hostPlatform.system}.pkgsi686Linux.mesa.drivers;
      };
    };

    home.activation.touch = ''
      touch ${config.home.homeDirectory}/.config/hypr/monitors.conf \
            ${config.home.homeDirectory}/.config/hypr/workspaces.conf
    '';

    xdg.configFile = {
      "swaync/config.json".text = builtins.toJSON {scripts = {};};
      # "hypr/shaders" = {
      #   source = ./shaders;
      #   recursive = true;
      # };
    };

    # Log rules: watch -n 0.1 "cat "/tmp/hypr/$(echo $HYPRLAND_INSTANCE_SIGNATURE)/hyprland.log" | grep -v "efresh" | grep "rule" | tail -n 40"
    wayland.windowManager.hyprland = {
      enable = true;
      systemd.enable = false;
      package = null;
      portalPackage = null;
      plugins = with inputs.hyprland-plugins.packages.${pkgs.stdenv.hostPlatform.system};
        [
          borders-plus-plus
          hyprwinwrap
          # inputs.hyprchroma.packages.${pkgs.stdenv.hostPlatform.system}.Hypr-DarkWindow
          # pkgs.hyprscroller # NOTE Doesn't work with git :(
          # pkgs.hypr-workspace-layouts
        ]
        ++ outputs.lib.optionals config.modules.hyprland.animations [
          inputs.hypr-dynamic-cursors.packages.${pkgs.stdenv.hostPlatform.system}.hypr-dynamic-cursors
        ];
      settings = {
        source = [
          "${config.home.homeDirectory}/.config/hypr/monitors.conf"
          "${config.home.homeDirectory}/.config/hypr/workspaces.conf"
        ];
        exec-once = [
          "${outputs.lib.getExe pkgs.sway-audio-idle-inhibit}"
          "${outputs.lib.getExe pkgs.kdePackages.xwaylandvideobridge}"
          "${outputs.lib.getExe pkgs.networkmanagerapplet}"
          "systemctl --user start hyprpolkitagent"
          "${hypr-gamemode}"
        ];
        # chromakey_background = "1,4,9";
        plugin = {
          wslayout = {
            default_layout = "master";
          };
          hyprwinwrap = {
            class = "mpv";
          };
          dynamic-cursors = outputs.lib.mkIf config.modules.hyprland.animations {
            enabled = true;
            mode = "tilt";
          };
        };
        general = {
          gaps_in = 5;
          gaps_out = 20;
          border_size = 1;
          "col.active_border" = "rgba(717585FF) rgba(707480FF) 90deg";
          "col.inactive_border" = "rgba(616977FF) rgba(636973FF) 90deg";

          # snap = {
          #   enabled = true;
          #   window_gap = 30;
          #   monitor_gap = 30;
          # };

          # layout = "workspacelayout";
          layout = "master";
          resize_on_border = false;
        };
        input = {
          kb_layout = "no";
          kb_options = "ctrl:nocaps";

          numlock_by_default = true;

          accel_profile = "flat";

          follow_mouse = 1;
          mouse_refocus = false;

          sensitivity = 0.5;

          touchpad = {
            natural_scroll = true;
            drag_lock = false;
            tap-and-drag = true;
          };
        };
        xwayland = {
          force_zero_scaling = true;
        };
        master = {
          new_status = "slave";
          allow_small_split = true;
          smart_resizing = false;
        };
        dwindle = {
          pseudotile = true;
          force_split = 2;
        };
        misc = {
          disable_hyprland_logo = true;
          force_default_wallpaper = 0;
          enable_swallow = true;
          key_press_enables_dpms = true;
          render_unfocused_fps = 30;
          new_window_takes_over_fullscreen = 1;
          swallow_regex = "^(Alacritty|kitty|org.wezfurlong.wezterm)$";
          animate_manual_resizes = config.modules.hyprland.animations;
          animate_mouse_windowdragging = config.modules.hyprland.animations;
        };
        decoration = {
          rounding = 7;

          blur = {
            enabled = true;
            size = 16;
            passes = 3;
            new_optimizations = true;
            ignore_opacity = true;
            vibrancy = 1;
            brightness = 1;
            xray = !config.modules.hyprland.animations;
            noise = 3.0e-2;
            contrast = 1;
          };

          shadow = {
            enabled = true;
            range = 32;
            render_power = 3;
            ignore_window = true;
            scale = 1;
            color = "rgba(00000055)";
            color_inactive = "rgba(00000028)";
          };

          layerrule = [
            "blur,wofi"
            "blur,launcher"
          ];
        };
        animations = {
          enabled = "yes";

          bezier = [
            "myBezier, 0.05, 0.9, 0.1, 1.05"
            "overshot,0.05,0.9,0.1,1.1"
          ];

          animation = [
            "windows, 1, 7, overshot"
            "windowsOut, 1, 7, default, popin 80%"
            "border, 1, 10, default"
            "borderangle, 1, 8, default"
            "fade, 1, 7, default"
            "workspaces, 1, 6, default"
          ];
        };
      };
    };
  }
