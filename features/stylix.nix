{
  inputs,
  lib',
  self,
  ...
}:
{
  flake.nixosModules.stylix =
    {
      config,
      lib,
      ...
    }:
    {
      imports = [ inputs.stylix.nixosModules.stylix ];

      options.features.stylix.wallpaper = lib.mkOption {
        type = lib.types.path;
        description = "Wallpaper used by the system (e.g. login manager).";
      };

      config.stylix = {
        image = config.features.stylix.image;
        targets = lib'.disable [
          "grub"
          "nixvim"
          "spicetify"
        ];
      };
    };

  flake.homeModules.stylix =
    {
      config,
      lib,
      pkgs,
      osConfig,
      ...
    }:
    let
      cfg = config.features.stylix;
    in
    {
      imports = [ inputs.stylix.homeModules.stylix ];

      options.features.stylix = {
        wallpaper = lib.mkOption {
          type = lib.types.path;
          default = osConfig.features.stylix.wallpaper;
          description = "Wallpaper used to generate the colour scheme.";
        };
        scheme = lib.mkOption {
          type = lib.types.str;
          default = "ayu-dark";
          description = "Base16 scheme name from base16-schemes.";
        };
        cursor = lib.mkOption {
          type = lib.types.enum [
            "bibata"
            "wii"
            "osu"
          ];
          default = "bibata";
        };
      };

      config =
        let
          cursors = {
            bibata = {
              package = pkgs.bibata-cursors;
              name = "Bibata-Modern-Classic";
            };
            wii = {
              package = self.packages.${pkgs.stdenv.hostPlatform.system}.wii-cursor;
              name = "Wii";
            };
            osu = {
              package = self.packages.${pkgs.stdenv.hostPlatform.system}.osu-cursor;
              name = "Osu";
            };
          };
        in
        {
          stylix = {
            enable = true;
            overlays.enable = false;
            polarity = "dark";
            image = cfg.wallpaper;
            base16Scheme = "${pkgs.base16-schemes}/share/themes/${cfg.scheme}.yaml";

            cursor = cursors.${cfg.cursor} // {
              size = 24;
            };

            fonts = {
              serif = {
                package = pkgs.libertinus;
                name = "Libertinus Serif";
              };
              sansSerif = {
                package = pkgs.inter;
                name = "Inter";
              };
              monospace = {
                package = pkgs.nerd-fonts.caskaydia-cove;
                name = "CaskaydiaCove Nerd Font";
              };
              emoji = {
                package = pkgs.noto-fonts-color-emoji;
                name = "Noto Color Emoji";
              };
              sizes = {
                terminal = 14;
                applications = 12;
                desktop = 10;
                popups = 10;
              };
            };

            opacity = {
              applications = 1.0;
              terminal = 0.8;
              desktop = 1.0;
              popups = 1.0;
            };

            # Disable targets we configure ourselves or don't use.
            targets = lib'.disable [
              "nixvim"
              "spicetify"
              "hyprland"
              "hyprlock"
              "vscode"
              "kitty"
              "waybar"
              "ghostty"
              "zed"
              "zen-browser"
              "nixvim"
            ];
          };

          gtk = {
            enable = true;
            gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
            gtk4.extraConfig.gtk-application-prefer-dark-theme = true;
          };

          home.sessionVariables = {
            XCURSOR_SIZE = config.stylix.cursor.size;
            HYPRCURSOR_SIZE = config.stylix.cursor.size;
          };

          xdg.systemDirs.data = [
            "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
            "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}"
          ];
        };
    };
}
