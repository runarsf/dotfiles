{
  config,
  outputs,
  ...
}:
outputs.lib.mkModule config "fastfetch" {
  programs.fastfetch = {
    enable = true;

    settings = {
      logo = {
        source = builtins.toFile "logo.txt" ''
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
        "break"
        "player"
        "media"
      ];
    };
  };
}
