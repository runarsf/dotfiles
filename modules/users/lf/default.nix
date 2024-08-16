{ outputs, config, pkgs, ... }:

outputs.lib.mkModule config "lf" {
  xdg.configFile."lf/icons".source = ./lficons;

  programs.lf = {
    enable = true;
    commands = {
      dragon-out = ''%${pkgs.xdragon}/bin/xdragon -a -x "$fx"'';
      mkdir = ''
        %{{
          mkdir -p "$@"
          lf -remote "send $id select \"$1\""
        }}
      '';
      mkfile = ''
        %{{
          touch "''${@}"
          lf -remote "send ''${id} select \"''${1}\""
        }}
      '';
      trash = "%set -f; ${pkgs.trashy}/bin/trash $fx";
    };

    keybindings = {
      "\\\"" = "";
      m = "";
      "'" = "";
      "\"" = "";
      d = "";
      c = "";
      f = "";
      t = "";
      md = "push :mkdir<space>";
      mf = "push :mkfile<space>";
      "+r" = ''$chmod +r "$f"'';
      "-r" = ''$chmod -r "$f"'';
      "+w" = ''$chmod +w "$f"'';
      "-w" = ''$chmod -w "$f"'';
      "+x" = ''$chmod +x "$f"'';
      "-x" = ''$chmod -x "$f"'';
      "." = "set hidden!";
      "<enter>" = "open";
      p = "paste";
      x = "cut";
      y = "copy";
      r = "rename";
      R = "reload";
      "<space>" = ":toggle; down";
      z = ''$zathura "$f" &'';
      "<esc>" = "quit";
      "q" = "quit";

      w = "";
      wp = ''$\${pkgs.swww}/bin/swww img --transition-type any --transition-step 100 --transition-fps 60 "$f"'';

      dr = "dragon-out";

      gg = "top";
      G = "bottom";
      gn = "cd ~/.config/nixos";
      gh = "cd";
      gr = "cd /";
      gtr = "cd ~/.local/share/Trash/files";

      dd = "trash";
      tr =
        "$ ${pkgs.trashy}/bin/trash list | ${pkgs.fzf}/bin/fzf --multi | awk '{$1=$1;print}' | rev | cut -d ' ' -f1 | rev | xargs ${pkgs.trashy}/bin/trash restore --match=exact --force";
      td =
        "$ ${pkgs.trashy}/bin/trash list | ${pkgs.fzf}/bin/fzf --multi | awk '{$1=$1;print}' | rev | cut -d ' ' -f1 | rev | xargs ${pkgs.trashy}/bin/trash empty --match=exact --force";

      e = ''$$EDITOR "$f"'';
      V = ''$''${pkgs.bat}/bin/bat --paging=always "$f"'';
    };

    settings = {
      # cleaner = builtins.toString ./lfclean;
      # previewer = builtins.toString ./lfpreview;
      shell = "bash";
      ifs = "\\n";
      shellopts = "-eu";
      preview = true;
      hidden = true;
      drawbox = true;
      icons = true;
      number = true;
      relativenumber = true;
      scrolloff = 4;
      ratios = "2:3:5";
      ignorecase = true;
    };

    extraConfig = ''
      # Dynamically allocate number of panes
      ''${{
        width="$(tput cols)"

        if test "''${width}" -le "80"; then
          lf -remote "send ''${id} set ratios 1:2"
        elif test "''${width}" -le "280"; then
          lf -remote "send ''${id} set ratios 1:2:3"
        else
          lf -remote "send ''${id} set ratios 1:2:3:5"
        fi
      }}
    '';
  };
}
