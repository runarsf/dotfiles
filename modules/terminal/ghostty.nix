{ lib', ... }:
{
  flake.homeModules.ghostty = lib'.mkModule "ghostty" {
    needs = [ "terminal" ];
    config =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        inherit (lib)
          mkEnableOption
          mkIf
          mkOption
          types
          optionals
          ;

        cfg = config.modules.ghostty;

        # GTK 4.20 stopped handling dead keys itself on Wayland when no input
        # method is running, so ¨ and ~ never reach the terminal.
        package = pkgs.symlinkJoin {
          name = "ghostty-${pkgs.ghostty.version}";
          paths = [ pkgs.ghostty ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/ghostty --set-default GTK_IM_MODULE simple
          '';
          inherit (pkgs.ghostty) meta;
          passthru = pkgs.ghostty.passthru // {
            inherit (pkgs.ghostty)
              man
              shell_integration
              terminfo
              vim
              ;
          };
        };

        bitmapFonts = [
          "scientifica"
          "Cozette"
        ];

        vectorFonts = [
          "JetBrains Mono"
          "JetBrainsMono Nerd Font"
          "CaskaydiaCove NFM"
        ];

        features = [
          "calt"
          "clig"
          "liga"
          "ss20"
          "cv02"
          "cv03"
          "cv04"
          "cv05"
          "cv06"
          "cv07"
          "cv11"
          "cv14"
          "cv15"
          "cv16"
          "cv17"
        ];
      in
      {
        options.modules.ghostty = {
          bitmapFont = mkEnableOption "the bitmap variant of the font";

          font = mkOption {
            type = types.nullOr (types.listOf types.str);
            default = null;
            description = "Replaces the whole fallback list; null takes the default for `bitmapFont`.";
          };

          fontSize = mkOption {
            type = types.nullOr types.number;
            default = null;
          };
        };

        config = {
          modules.terminal.terminals.ghostty = {
            package = config.programs.ghostty.package;
            class = class: [ "--class=${class}" ];
          };

          # The unit ships inside the package and runs the unwrapped binary.
          xdg.configFile."systemd/user/app-com.mitchellh.ghostty.service.d/im.conf" =
            mkIf config.programs.ghostty.systemd.enable
              {
                text = ''
                  [Service]
                  Environment=GTK_IM_MODULE=simple
                '';
              };

          programs.ghostty = {
            enable = true;
            inherit package;
            settings = {
              theme = "dark:Ayu,light:Ayu Light";
              window-theme = "ghostty";
              cursor-style-blink = false;
              cursor-style = "bar";

              working-directory = "home";
              window-inherit-working-directory = false;
              tab-inherit-working-directory = false;
              split-inherit-working-directory = true;

              keybind = [
                "alt+c=copy_to_clipboard"
                "alt+v=paste_from_clipboard"
                "ctrl+a>|=new_split:right"
                "ctrl+a>-=new_split:down"
                "ctrl+a>z=toggle_split_zoom"
                "shift+left=goto_split:left"
                "shift+right=goto_split:right"
                "shift+up=goto_split:up"
                "shift+down=goto_split:down"
              ];

              font-family =
                if cfg.font != null then
                  cfg.font
                else if cfg.bitmapFont then
                  bitmapFonts
                else
                  vectorFonts;
              font-size = if cfg.fontSize != null then cfg.fontSize else 14;
              # Only the vector fonts have these.
              font-feature = optionals (!cfg.bitmapFont && cfg.font == null) features;
            };
          };
        };
      };
  };
}
