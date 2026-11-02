{ lib, ... }:
{
  shell = {
    # Every man1/man8 page in `packages`, converted to zsh completions. Nix
    # caches the result, so it only reruns when one of those packages changes.
    mkManCompletions =
      {
        pkgs,
        generator,
        packages,
      }:
      pkgs.runCommand "zsh-man-completions"
        {
          nativeBuildInputs = [
            pkgs.carapace
            pkgs.fish
            pkgs.jq
            pkgs.python3
            generator
          ];
        }
        ''
          shopt -s nullglob
          export HOME="$NIX_BUILD_TOP"

          # site-functions rather than $out directly: compaudit rejects an
          # $fpath entry whose parent is group-writable, and /nix/store is.
          dst="$out/share/zsh/site-functions"
          mkdir -p fish converted "$dst"

          # fish 4 keeps the parser inside the binary.
          fish -c 'status get-file tools/create_manpage_completions.py' > parser.py
          python3 -B parser.py --directory=fish ${
            lib.concatStringsSep " " (lib.unique (map (p: "${p.man or p}/share/man/man[18]/*") packages))
          }

          zsh-manpage-completion-generator -src fish -dst converted

          # Man pages only yield option names and short blurbs, so anything zsh
          # or carapace already completes is better left to them. Keeping only
          # the rest also means a reordered $fpath cannot shadow them.
          {
            carapace --list | jq -r 'keys[]'
            head -qn1 ${pkgs.zsh}/share/zsh/*/functions/_* | sed -n 's/^#compdef //p' | tr ' ' '\n'
          } | sed 's/=.*//' | grep -v '^-' | sort -u > covered
          ls converted | sed 's/^_//' | sort > generated

          comm -23 generated covered | while read -r cmd; do
            cp "converted/_$cmd" "$dst/_$cmd"
          done
        '';

    mkZoxideInit =
      pkgs: shellName:
      pkgs.runCommand "zoxide-init-${shellName}" { }
        "${pkgs.lib.getExe pkgs.zoxide} init ${shellName} > $out";
    mkStarshipInit =
      pkgs: shellName:
      pkgs.runCommand "starship-init-${shellName}" { }
        "${pkgs.lib.getExe pkgs.starship} init ${shellName} > $out";
    # home-manager emits these as `eval "$(... init <shell>)"`, a process spawn
    # per startup. The output is static, so bake it at build time instead.
    # carapace is the exception: it bakes $XDG_CONFIG_HOME into its snippet, so
    # keep that an expansion or the build sandbox's value leaks into the shell.
    mkCarapaceInit =
      pkgs: shellName:
      pkgs.runCommand "carapace-init-${shellName}" { } ''
        XDG_CONFIG_HOME=/@XDG@ ${pkgs.lib.getExe pkgs.carapace} _carapace ${shellName} \
          | sed 's|/@XDG@|''${XDG_CONFIG_HOME:-$HOME/.config}|' > $out
      '';
    mkDirenvHook =
      pkgs: shellName:
      pkgs.runCommand "direnv-hook-${shellName}" { }
        "${pkgs.lib.getExe pkgs.direnv} hook ${shellName} > $out";
    # `enableLsColors` would eval `dircolors` on every startup; this is the
    # same output, and completion list-colors needs $LS_COLORS to exist.
    mkLsColors =
      pkgs: pkgs.runCommand "ls-colors" { } "${pkgs.lib.getExe' pkgs.coreutils "dircolors"} -b > $out";

    greeting = pkgs: "${pkgs.lib.getExe pkgs.krabby} random --no-title";

    aliases = pkgs: {
      tree = "${pkgs.lib.getExe pkgs.eza} --tree";
      cat = "${pkgs.lib.getExe pkgs.bat}";
      grep = "grep --color=always";
      docker-compose = "docker compose";
      dkcUf = "docker compose up -d --force-recreate";
      ls = "EZA_ICON_SPACING=2 ${pkgs.lib.getExe pkgs.eza} -l -F -g -a --group-directories-first --no-time --git";
      develop = "nix develop --command $SHELL";
    };

    abbrs = {
      nh = "niks";
    };
  };
}
