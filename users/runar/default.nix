{
  config,
  inputs,
  outputs,
  system,
  hostname,
  name,
  pkgs,
  ...
}:
outputs.lib.mkFor system hostname {
  common = {
    # TODO Make this apply to all users
    imports =
      outputs.lib.concatPaths {paths = ../../modules/users;}
      ++ outputs.lib.concatPaths {paths = ./config;};

    # TODO: Refactor implementation
    defaultTerminal = "wezterm";
    # defaultBrowser = "zen";
    avatar = ./avatar.jpg;

    modules =
      outputs.lib.enable [
        "neovim"
        "zsh"
        "gpg"
        "keychain"
        "nix"
        "yazi"
        "fun"
        "fastfetch"
      ]
      // {
        wallpaper = ./wallpapers/jars.jpg;
        sops = {
          enable = true;
          ageKeys = ["${config.home.homeDirectory}/.ssh/id_nix"];
          privateKeys = [
            "id_priv"
            "id_ntnu"
          ];
        };
        ssh = {
          enable = true;
          signingKey = "id_priv";
          keys = {
            id_nix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGwThBXxJMvEDSf/WUlXtgvs+R5TTZwILnAvCp5Zl02Z nix";
            id_priv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBT5zQFdVRooe5SfFZ2gKpruHF7FTw1OycTczRrLsR+M i@runar.ch";
            id_ntnu = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO6Y4kk5hFzs/B6vze9u9RPG9d+vVM5EIRIOug4OnJBk runarsfr@stud.ntnu.no";
          };
        };
      };

    wayland.windowManager.hyprland.settings = {
      workspace = let
        primary = "monitor:DP-2;DP-3;DP-4;HDMI-A-2";
        secondary = "monitor:eDP-1";
      in [
        "1, ${primary}"
        "10, ${primary}"
        "2, ${secondary}"
      ];
      # hyprctl -j devices | jq -r '.mice | .[] | .name'
      device = outputs.lib.mkDefault [
        {
          name = "logitech-mx-ergo-1";
          sensitivity = "0.6";
        }
        {
          name = "logitech-pro-x-2";
          sensitivity = "-0.4";
        }
        {
          name = "logitech-pro-x-2-1";
          sensitivity = "-0.4";
        }
        {
          name = "logitech-pro-x-2-2";
          sensitivity = "-0.4";
        }
        {
          name = "logitech-usb-receiver";
          sensitivity = "-0.4";
        }
      ];
    };
  };

  systems = {
    linux = {
      modules =
        outputs.lib.enable [
          "discord"
          "fonts"
          "vscode"
          "zed"
          "writing"
          "spotify"
          "sops-fonts"
          "bluetooth"
          "pipewire"
          "mpv"
          ["dev" "podman"]
          "camera"
          "zen"
          "logitech"
          "localsend"
          "noctalia"
          "vicinae"
        ]
        // {
          matrix = {
            enable = true;
            clients = outputs.lib.enable [
              "element"
              "commet"
            ];
          };
          hyprland = {
            enable = true;
            animations = false;
          };
          stylix = {
            enable = true;
            system-wide = true;
            theme = "ayu-dark";
          };
          wezterm = {
            enable = true;
            bitmap = true;
          };
          flatpak.enable = config.isDesktop;
        };

      home.packages = with pkgs;
        outputs.lib.optionals config.isDesktop [
          obs-studio
          chromium
          feishin
          fluffychat
          # openconnect-sso --server vpn.ntnu.no
          inputs.openconnect-sso.packages."${pkgs.stdenv.hostPlatform.system}".default
        ];

      nixos = {
        programs.zsh.enable = true;
        users.users."${name}" = {
          isNormalUser = true;
          initialPassword = "changeme";
          description = "Runar";
          home = "/home/runar";
          shell = pkgs.zsh;
          extraGroups = [
            "wheel"
            "networkmanager"
            "docker"
            "audio"
            "video"
            "libvirtd"
            "input"
            "i2c"
            "blahaj"
          ];
        };
        # https://discourse.nixos.org/t/howto-disable-most-gnome-default-applications-and-what-they-are/13505/14
        environment.gnome.excludePackages = with pkgs; [
          # baobab # disk usage analyzer
          cheese # photo booth
          eog # image viewer
          epiphany # web browser
          # gedit # text editor
          simple-scan # document scanner
          totem # video player
          yelp # help viewer
          evince # document viewer
          file-roller # archive manager
          geary # email client
          # seahorse # password manager

          gnome-calculator
          gnome-calendar
          # gnome-characters
          gnome-clocks
          gnome-contacts
          # gnome-font-viewer
          # gnome-logs
          # gnome-maps
          gnome-music
          gnome-photos
          gnome-screenshot
          gnome-system-monitor
          gnome-weather
          # gnome-disk-utility
          gnome-connections # Remote Desktop Client
        ];
      };
    };
  };

  hosts = {
    runix = {
      isDesktop = true;

      # FIXME Using dev.go and dev.go.ide in lib.enable breaks and neither get enabled
      modules =
        outputs.lib.enable [
          "audiorelay"
          "kvm"
          "easyeffects"
          "qmk"
        ]
        // {
          dev = {
            python = {
              enable = true;
              packageName = "python311";
            };
            go.enable = true;
            android = {
              enable = true;
              ide = true;
            };
          };
        };

      programs.element-desktop.enable = true;

      home.packages = with pkgs; [
        pokemmo-installer
        telegram-desktop
      ];

      # xdg.desktopEntries."steam-handler" = {
      #   type = "Application";
      #   name = "Steam Handler";
      #   mimeType = [ "x-scheme-handler/steam" ];
      #   exec = "${
      #       pkgs.writeShellScript "steam-handler" ''
      #         #!/run/current-system/sw/bin/bash
      #         set -o errexit
      #         set -o nounset
      #         notify-send "Steam trynna open $1"
      #       ''
      #     } %u";
      # };
    };

    anuc = {
      # TODO: Maybe mkDefault this for all hosts and mkForce for desktops, see TODO in flake.nix
      isDesktop = false;

      modules = outputs.lib.enable [
        "tmux"
        "docker"
      ];
    };

    rpi = {
      isDesktop = false;
    };
  };
}
