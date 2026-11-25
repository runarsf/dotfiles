{ lib', ... }:
{
  flake.homeModules.ghostty = lib'.mkModule "ghostty" {
    needs = [ "terminal" ];
    config =
      { config, lib, ... }:
      let
        inherit (lib)
          mkEnableOption
          mkOption
          types
          optionals
          ;

        cfg = config.modules.ghostty;

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

          programs.ghostty = {
            enable = true;
            settings = {
              theme = "Ayu";
              cursor-style = "bar";

              keybind = [
                "alt+c=copy_to_clipboard"
                "alt+v=paste_from_clipboard"
                "ctrl+a>|=new_split:right"
                "ctrl+a>-=new_split:down"
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
