{
  config,
  inputs,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config "fastfetch" {
  programs.fastfetch = {
    enable = true;

    # package = pkgs.writers.writeNuBin "fastfetch" ''
    #   def main --wrapped [...args] {
    #     echo ...$args
    #   }
    #
    #   let left_pad = open ~/.config/fastfetch/config.jsonc | from json | get --optional logo?.padding?.left?
    #   if left_pad == null {
    #     left_pad = 6
    #   }
    #   left_pad = left_pad / 2 | math ceil
    #
    #   ${builtins.toJSON (import (inputs.nixvim + "/modules/quotes.nix"))} | from json
    #   printf "%s\n" "$multiline" | awk -v pad="$left_pad" '{printf "%*s%s\n", pad, "", $0}'
    # '';

    settings = {
      logo = {
        source = outputs.lib.mkDefault <| builtins.toFile "logo.txt" ''
           $2..$1^____/
          `-. ___ )
            ||  ||
        '';
        color = {
          "1" = "blue";
          "2" = "white";
        };
        padding = {
          top = 2;
          left = 6;
          right = 6;
        };
      };
      display = {
        separator = " ";
        color = {
          keys = "magenta";
          title = "white";
        };
      };
      modules = [
        "break"
        {
          type = "os";
          key = "󱄅 OS";
        }
        {
          type = "editor";
          key = " Editor";
        }
        # {
        #   type = "packages";
        #   key = " Packages";
        # }
        {
          type = "uptime";
          key = "󰥔 Uptime";
        }
        {
          type = "command";
          key = "󱦟 OS Age";
          text = "birth_install=$(stat -c %W /); current=$(date +%s); time_progression=$((current - birth_install)); days_difference=$((time_progression / 86400)); echo $days_difference days";
        }
      ];
    };
  };
}
