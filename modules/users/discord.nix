{
  outputs,
  config,
  pkgs,
  ...
}:

# ~/.config/Vencord/themes

outputs.lib.mkDesktopModule config "discord" {
  home.packages = with pkgs; [ discord ];
  # if (outputs.lib.isWayland config) then [ waycord vesktop ] else [ discord ];

  xdg.configFile = {
    "Vencord/themes/SettingsModal.theme.css".source = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/mwittrien/BetterDiscordAddons/d5b8dd17692e1f30edf94170e4be0f030fbc9bc5/Themes/SettingsModal/SettingsModal.css";
      sha256 = "sha256-F0v8VvYrHCbmfDjKxlxliwJul6LOChL9iBvoORIYog0=";
    };
    "Vencord/themes/midnight.theme.css".source = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/refact0r/midnight-discord/bc6c15487eb6a416b0914d8e5b48af47f7608ffe/midnight.css";
      sha256 = "sha256-6UgSgsn6Kb5w2V1VdHMxipQ5c1W5Zm8Jbx9GefK89oo=";
    };
  };

  nixpkgs.overlays = [
    (final: prev: {
      discord = prev.discord.override {
        withOpenASAR = true;
        withVencord = true;
      };
      # waycord = prev.symlinkJoin {
      #   name = "discord";
      #   paths = [
      #     (prev.writeShellScriptBin "discord" ''
      #       exec ${final.discord}/bin/discord \
      #         --enable-features=UseOzonePlatform,WaylandWindowDecorations \
      #         --ozone-platform-hint=auto \
      #         --ozone-platform=wayland \
      #         "$@"
      #     '')
      #     final.discord
      #   ];
      # };
    })
  ];
}
