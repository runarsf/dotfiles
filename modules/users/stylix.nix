{
  outputs,
  config,
  pkgs,
  name,
  ...
}:
# FIXME For some reason, every other build stylix themes are light / dark themed...?
# TODO Refactor for home manager integration https://stylix.danth.me/options/nixos.html#stylixhomemanagerintegrationautoimport
let
  stylix-config = {
    enable = true;

    polarity = "dark";
    image = config.modules.wallpaper;

    targets =
      outputs.lib.disable ["nixvim"]
      // {
        qt.platform = "qtct";
      };

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Classic";
      size = 24;
    };

    fonts = {
      monospace = {
        package = pkgs.nerd-fonts.caskaydia-cove;
        name = "Cascadia Mono NF";
      };

      sansSerif = {
        package = pkgs.dejavu_fonts;
        name = "DejaVu Sans";
      };

      serif = {
        package = pkgs.libertine;
        name = "Linux Libertine O";
      };

      sizes = {
        terminal = 14;
        applications = 12;
        desktop = 10;
        popups = 10;
      };

      emoji = {
        package = pkgs.noto-fonts-emoji;
        name = "Noto Color Emoji";
      };
    };

    opacity = {
      applications = 1.0;
      terminal = 0.8;
      desktop = 1.0;
      popups = 1.0;
    };

    base16Scheme = "${pkgs.base16-schemes}/share/themes/${config.modules.stylix.theme}.yaml";
  };

  settings = {
    user = rec {
      stylix = outputs.lib.deepMerge [
        stylix-config
        {
          targets = outputs.lib.disable [
            "vscode"
            "hyprland"
            "kitty"
            "waybar"
            "hyprlock"
            "spicetify"
          ];
        }
      ];

      # This needs to always be set for the Stylix system configuation to be valid,
      # even if Stylix is disabled system-wide
      nixos.stylix.image = stylix.image;

      gtk = {
        enable = true;
        gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
        gtk4.extraConfig.gtk-application-prefer-dark-theme = true;
      };

      home.sessionVariables = {
        XCURSOR_SIZE = stylix-config.cursor.size;
        HYPRCURSOR_SIZE = stylix-config.cursor.size;
      };

      xdg.systemDirs.data = [
        "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
        "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}"
      ];

      # systemd.user.services.rm-gtk = {
      #   Unit = {
      #     Description = "Remove GTK files built by Home Manager";
      #     PartOf = [ "home-manager-${name}.target" ];
      #   };

      #   Service.ExecStart = builtins.toString (pkgs.writeShellScript "rm-gtk" ''
      #     #!/run/current-system/sw/bin/bash
      #     set -o errexit
      #     set -o nounset

      #     printf "Removing GTK files built by Home Manager\n"

      #     rm -rf ~/.config/gtk-3.0
      #     rm -rf ~/.config/gtk-4.0
      #     rm -f .gtkrc-2.0
      #   '');

      #   Install.WantedBy = [ "default.target" ];
      # };
    };

    system-wide = {
      stylix = outputs.lib.deepMerge [
        stylix-config
        {
          targets = outputs.lib.disable ["grub"];
        }
      ];
    };
  };
in {
  options = {
    modules = {
      wallpaper = outputs.lib.mkOption {
        type = outputs.lib.types.path;
        default = ./assets/wp-default-dark.png;
        description = "Path to the wallpaper.";
      };

      stylix = {
        enable = outputs.lib.mkEnableOption "Enable Stylix";
        system-wide = outputs.lib.mkEnableOption "Enable Stylix system-wide. This will install Stylix for all users.";

        # https://github.com/tinted-theming/schemes
        theme = outputs.lib.mkOption {
          type = outputs.lib.types.str;
          default = "ayu-dark";
          description = "The Base16 theme to use.";
        };
      };
    };
  };

  config = outputs.lib.mkIf config.modules.stylix.enable (
    outputs.lib.mkMerge [
      # TODO Is there not user-config that we still want on the system-wide version
      (outputs.lib.mkIf config.modules.stylix.system-wide {
        nixos = outputs.lib.trace "info: Enabling Stylix system-wide. This will override the configs of all users with ${name}'s config." settings.system-wide;
      })
      (outputs.lib.mkIf (!config.modules.stylix.system-wide) settings.user)
      {
        # FIXME Stylix supports hyprlock now, but is too dumb to apply it correctly
        programs.hyprlock.settings.background.path = builtins.toString config.modules.wallpaper;

        services.hyprpaper.settings = {
          preload = ["${config.modules.wallpaper}"];
          wallpaper = [", ${config.modules.wallpaper}"];
        };
      }
    ]
  );
}
