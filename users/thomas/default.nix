{
  config,
  inputs,
  osConfig,
  outputs,
  system,
  hostname,
  name,
  pkgs,
  ...
}: let
  ifIsDesktop = outputs.lib.optionals config.isDesktop; # (outputs.lib.isDesktop config hostname);
in
  outputs.lib.mkFor system hostname {
    common = {
      imports =
        outputs.lib.concatPaths {paths = ../../modules/users;}
        ++ outputs.lib.concatPaths {paths = ./config;};

      defaultTerminal = "wezterm";
      defaultBrowser = "zen";
      avatar = ./avatar.jpg;

      modules =
        outputs.lib.enable [
          "neovim"
          "zsh"
          "nushell"
          "gpg"
          "keychain"
          "nix"
          "yazi"
        ]
        // {
          wallpaper = ./wallpaper.jpg;
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
            keys = {
              id_nix = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFw8lBpuv2bWKYxxXeeG6pZ7Ut2GCtjuEbuvVEp9DmeY nix";
              id_priv = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8glmBsdfxRsQxzZrljQynBF09jljQD4KIH33Kcx9Hw thoesp@protonmail.com";
              id_ntnu = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBYghkkwi+HG+q91Xhcdc+Ac8wYdIo8BzUZKUPa2/00f thomes@stud.ntnu.no";
            };
          };
        };
    };

    systems = {
      linux = {
        modules =
          outputs.lib.enable [
            "discord"
            "fonts"
            "vscode"
            "hyprland"
            "kitty"
            "writing"
            "sops-fonts"
            "bluetooth"
            "zen"
            "pipewire"
            "mpv"
            "camera"
            "fastfetch"
          ]
          // {
            spotify = {
              enable = true;
              spicetify = true;
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
            dev = {
              c = {
                enable = true;
                ide = true;
              };
              python = {
                enable = true;
                packageName = "python311";
                presets = outputs.lib.enable [
                  "math"
                  "jupyter"
                ];
              };
            };
          };

        home.packages = with pkgs; ifIsDesktop [inputs.openconnect-sso.packages."${pkgs.system}".default];

        programs.fastfetch.settings.logo = {
          source =
            outputs.lib.mkForce
            <| builtins.toFile "logo.txt" ''
               ／|_
              ($2o o$1 /
               |.   ~.
               じしf_,)ノ
            '';
          padding.top = outputs.lib.mkForce 1;
        };
        nixos = {
          programs.zsh.enable = true;
          users.users."${name}" = {
            isNormalUser = true;
            initialPassword = "changeme";
            description = "Thomas";
            home = "/home/thomas";
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
            ];
          };
        };
      };
    };

    hosts = {
      boiler = {
        isDesktop = true;

        modules =
          outputs.lib.enable [
            "japanese"
            "steam"
            "ffxiv"
            "hytale"
            "fun"
            "reaper"
            "qmk"
            "easyeffects"
            "zed"
          ]
          // {
            dev = {
              rust = {
                enable = true;
                ide = true;
              };
              haskell.enable = true;
              java.enable = true;
              # python.packages = with pkgs.python311Packages; [manim];
            };
            udev.extraRules = [../../modules/users/gaming/dualsense.rules];
          };

        home.packages = with pkgs; [
          prismlauncher
          osu-lazer-bin
          rpcs3
          r2modman
        ];

        nixos = {
          environment.systemPackages = with pkgs; [amdgpu_top corectrl];
          hardware = {
            amdgpu.initrd.enable = true;
            graphics = {
              enable = true;
              enable32Bit = true;
              package = with pkgs; mesa;
              # extraPackages = with pkgs; [ amdvlk mesa ];
            };

            bluetooth.powerOnBoot = true;
            opentabletdriver.enable = true;
          };

          services = {
            xserver.videoDrivers = ["modesetting"];
            tailscale.enable = true;
          };

          # systemd.tmpfiles.rules = [
          #   "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
          # ];

          boot = {
            kernelPackages = pkgs.linuxPackages_latest;
            initrd.kernelModules = ["amdgpu"];
            # kernelParams = [
            #   "radeon.si_support=0"
            #   "amdgpu.si_support=1"
            #   "amdgpu.dc=1"
            # ];

            kernel.sysctl = {
              "fs.file-max" = 262144;
            };
          };

          security.pam.loginLimits = [
            {
              domain = "*";
              item = "nofile";
              type = "-";
              value = "262144";
            }
            {
              domain = "*";
              item = "memlock";
              type = "-";
              value = "262144";
            }
          ];
          systemd = {
            user.extraConfig = "DefaultLimitNOFILE=262144";
          };
        };

        wayland.windowManager.hyprland.settings = {
          input.tablet.left_handed = true;
          bind = [
            ",Pause,exec,/nix/store/di3glqanq4y9fxwxmpmypaw3rvlkdwf1-wireplumber-0.5.8/bin/wpctl set-mute @DEFAULT_SOURCE@ toggle"
            "SUPER,P,exec,hyprctl keyword monitor DP-1,disable"
          ];
          env = [
            "GDK_BACKEND,wayland,x11"
            "SDL_VIDEODRIVER,wayland,x11"
            "CLUTTER_BACKEND,wayland"
            "MOZ_ENABLE_WAYLAND,1"
            "MOZ_DISABLE_RDD_SANDBOX,1"
            "_JAVA_AWT_WM_NONREPARENTING=1"
            "QT_AUTO_SCREEN_SCALE_FACTOR,1"
            "QT_QPA_PLATFORM,wayland"
            # "LIBVA_DRIVER_NAME,nvidia"
            # "GBM_BACKEND,nvidia-drm"
            # "__GLX_VENDOR_LIBRARY_NAME,nvidia"
            "WLR_NO_HARDWARE_CURSORS,1"
            "__NV_PRIME_RENDER_OFFLOAD,1"
            # "__VK_LAYER_NV_optimus,NVIDIA_only"
            "PROTON_ENABLE_NGX_UPDATER,1"
            "NVD_BACKEND,direct"
            "__GL_GSYNC_ALLOWED,0"
            "__GL_VRR_ALLOWED,0"
            "WLR_EGL_NO_MODIFIERS,1"
            # "WLR_DRM_NO_ATOMIC,1"
            "WLR_USE_LIBINPUT,1"
            # "XWAYLAND_NO_GLAMOR,1" # with this you'll need to use gamescope for gaming
            "__GL_MaxFramesAllowed,1"
            # "WLR_RENDERER_ALLOW_SOFTWARE,1"
          ];

          windowrule = [
            "workspace 5, match:class XIVLauncher.Core"
            "workspace 5, match:class Ffxiv_dx11.exe"
            "workspace 5, match:class HytaleClient"
          ];

          workspace = let
            primary = "monitor:DP-1";
            secondary = "monitor:HDMI-A-1";
          in [
            "4, ${primary}"
            "10, ${primary}"
            "1, ${secondary}"
            "2, ${secondary}"
          ];
        };
      };

      toaster = {
        isDesktop = true;

        modules =
          outputs.lib.enable [
            "japanese"
            "ctf"
            "steam"
            "gaming"
            "fun"
            "docker"
          ]
          // {
            dev = {
              android = {
                enable = true;
                ide = true;
              };
              haskell.enable = true;
              rust.enable = true;
              # python.packages = with pkgs.python311Packages; [manim];
            };
          };
      };
    };
  }
