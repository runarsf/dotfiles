{
  outputs,
  config,
  pkgs,
  ...
}: let
  discordPatcher = pkgs.writers.writePython3Bin "discord-krisp-patcher" {
    libraries = with pkgs.python3Packages; [
      pyelftools
      capstone
    ];
    flakeIgnore = [
      "E265" # from nix-shell shebang
      "E501" # line too long (82 > 79 characters)
      "F403" # ‘from module import *’ used; unable to detect undefined names
      "F405" # name may be undefined, or defined from star imports: module
    ];
  } (builtins.readFile ./discord-krisp-patcher.py);
in
  outputs.lib.mkDesktopModule config "discord" {
    # NOTE https://github.com/NixOS/nixpkgs/issues/195512
    home.activation.krispPatch = config.lib.dag.entryAfter ["writeBoundary"] ''
      run ${pkgs.findutils}/bin/find -L ${config.home.homeDirectory}/.config/discord -name 'discord_krisp.node' -exec ${discordPatcher}/bin/discord-krisp-patcher {} \;
    '';

    home.packages = with pkgs; [
      vesktop
      discord
      # (discord-ptb.override {
      #   withOpenASAR = true;
      #   withVencord = true;
      # })
    ];

    xdg.configFile = {
      "Vencord/themes/SettingsModal.theme.css".source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/477ca52e19a38cbd8fe29b9c537afaee54224fbf/Themes/SettingsModal/SettingsModal.theme.css";
        sha256 = "sha256-ib6y+L5uD+y27THA4OwrIKulgC3otrwZidGRtFdIHWc=";
      };
      "Vencord/themes/midnight-refresh.theme.css".source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/refact0r/midnight-discord/3a51cd37870915a8e2385bd63e40b30f382e7957/themes/midnight-refresh.theme.css";
        sha256 = "sha256-TwIXNkjIc+tghRtfaB46u5aEaxUYsIyPoWASBFJMD5c=";
      };
      "Vencord/themes/midnight.theme.css".source = pkgs.fetchurl {
        url = "https://raw.githubusercontent.com/refact0r/midnight-discord/3a51cd37870915a8e2385bd63e40b30f382e7957/midnight.theme.css";
        sha256 = "sha256-O33tuRKGgsuiEkpSRtVY+Wq9+gKNWNUbYLspQ6uPnII=";
      };
      "Vencord/themes/system24.theme.css".source = let
        hash = "a6e86edf00144f2fe740b3592494cdd9ea2c3f66";
      in
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/refact0r/system24/${hash}/theme/system24.theme.css";
          sha256 = "sha256-gpHaLfWBOJjt/P+nvs69I6H8ceUVTetMrHCJKdJxHzc=";
        };
      "Vencord/themes/GitHub-Dark.theme.css".source = let
        hash = "615b99a421fe40940a592fce0e3dbc47ef585ab4";
      in
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/moistp1ckle/GitHub_Dark/${hash}/source.css";
          sha256 = "sha256-/m/PN4AZGdEotqBcKQTkRSdSpI7iiL0s/CeAA3hmwx0=";
        };
      "Vencord/themes/RadialStatus.theme.css".source = let
        hash = "8444d415c44d7019708eb0a577b085141725a2df";
      in
        pkgs.fetchurl {
          url = "https://raw.githubusercontent.com/DiscordStyles/RadialStatus/${hash}/RadialStatus.theme.css";
          sha256 = "sha256-R8dxgZovZe92n5lNNyxBTOxhuQduyszj+nrx3kafAJ4=";
        };
      # https://betterdiscord.app/theme/Solana
      "Vencord/themes/BitmapFont.theme.css".text = ''
        /**
         * @name BitmapFont
         * @description Use the Monocraft font
         */
        *:not([class*="hljs"]):not(code),
        code {
          font-family: 'Monocraft', 'Operator Mono Lig' !important;
        }

        /* code {
          font-family: 'Operator Mono Lig' !important;
        } */
      '';
    };
  }
