{ outputs, config, pkgs, ... }:

let
  discordPatcher = pkgs.writers.writePython3Bin "discord-krisp-patcher" {
    libraries = with pkgs.python3Packages; [ pyelftools capstone ];
    flakeIgnore = [
      "E265" # from nix-shell shebang
      "E501" # line too long (82 > 79 characters)
      "F403" # ‘from module import *’ used; unable to detect undefined names
      "F405" # name may be undefined, or defined from star imports: module
    ];
  } (builtins.readFile ./discord-krisp-patcher.py);

in outputs.lib.mkDesktopModule config "discord" {
  # NOTE https://github.com/NixOS/nixpkgs/issues/195512
  home.activation.krispPatch = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    run ${pkgs.findutils}/bin/find -L ${config.home.homeDirectory}/.config/discord -name 'discord_krisp.node' -exec ${discordPatcher}/bin/discord-krisp-patcher {} \;
  '';

  home.packages = with pkgs.master;
    if (outputs.lib.isWayland config) then [
      discord-wayland
      vesktop
    ] else
      [ discord ];

  xdg.configFile = {
    "Vencord/themes/SettingsModal.theme.css".source = pkgs.fetchurl {
      url =
        "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/d5b8dd17692e1f30edf94170e4be0f030fbc9bc5/Themes/SettingsModal/SettingsModal.css";
      sha256 = "sha256-F0v8VvYrHCbmfDjKxlxliwJul6LOChL9iBvoORIYog0=";
    };
    "Vencord/themes/midnight.theme.css".source = pkgs.fetchurl {
      url =
        "https://raw.githubusercontent.com/refact0r/midnight-discord/bc6c15487eb6a416b0914d8e5b48af47f7608ffe/midnight.css";
      sha256 = "sha256-6UgSgsn6Kb5w2V1VdHMxipQ5c1W5Zm8Jbx9GefK89oo=";
    };
    "Vencord/themes/system24.theme.css".source = pkgs.fetchurl {
      url =
        "https://raw.githubusercontent.com/refact0r/system24/main/theme/system24.theme.css";
      sha256 = "sha256-WJYNWeo1DdgOh7cFne6QSEHQagqapFznqWfeoCsqM+8=";
    };
    "Vencord/themes/CustomFonts.theme.css".source = builtins.toString
      (pkgs.writeText "CustomFonts.theme.css" ''
        *:not([class*="hljs"]):not(code),
        code {
          font-family:  'Monocraft', 'Operator Mono Lig' !important;
        }

        /* code {
          font-family: 'Operator Mono Lig' !important;
        } */
      '');
  };

  nixpkgs.overlays = [
    (_: prev: {
      discord-wayland = let
        discord = prev.discord.override {
          withOpenASAR = true;
          withVencord = true;
        };
      in prev.symlinkJoin {
        name = "Discord";
        paths = [
          (prev.writeShellScriptBin "Discord" ''
            exec ${discord}/bin/Discord \
              --enable-features=UseOzonePlatform,WaylandWindowDecorations \
              --ozone-platform-hint=auto \
              --ozone-platform=wayland \
              "$@"
          '')
          discord
        ];
      };
    })
  ];
}
