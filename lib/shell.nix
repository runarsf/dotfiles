{ lib, ... }:
{
  shell = {
    # Man pages in `packages` turned into zsh completions. `commands` have no
    # man page, so help2man builds one from `--help`.
    mkDocCompletions =
      {
        pkgs,
        generator,
        packages,
        commands,
      }:
      pkgs.runCommand "zsh-doc-completions"
        {
          nativeBuildInputs = [
            pkgs.carapace
            pkgs.fish
            pkgs.help2man
            pkgs.jq
            pkgs.python3
            generator
          ];
        }
        ''
          shopt -s nullglob
          export HOME="$NIX_BUILD_TOP"
          export PATH="${lib.makeBinPath packages}:$PATH"

          # site-functions rather than $out directly: compaudit rejects an
          # $fpath entry whose parent is group-writable, and /nix/store is.
          dst="$out/share/zsh/site-functions"
          mkdir -p fish converted help "$dst"

          # --version-string so help2man does not run each command twice.
          for cmd in ${lib.escapeShellArgs commands}; do
            timeout 20 help2man --no-info --no-discard-stderr --version-string=0 \
              --output="help/$cmd.1" "$cmd" < /dev/null
          done

          # fish 4 keeps the parser inside the binary.
          fish -c 'status get-file tools/create_manpage_completions.py' > parser.py
          python3 -B parser.py --directory=fish ${
            lib.concatStringsSep " " (lib.unique (map (p: "${p.man or p}/share/man/man[18]/*") packages))
          } help/*.1

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

          # `commands` skip the filter -- nothing else completes them anyway.
          for cmd in ${lib.escapeShellArgs commands}; do
            if [[ ! -e "converted/_$cmd" ]]; then
              echo "no options parsed out of \`$cmd --help'" >&2
              exit 1
            fi
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
    #
    # It also grabs ~800 commands in one compdef, hiding the completions
    # packages ship themselves. Drop the ones we already have a `_<cmd>` for.
    mkCarapaceZshInit =
      {
        pkgs,
        packages,
      }:
      pkgs.runCommand "carapace-init-zsh" { } ''
        shopt -s nullglob

        XDG_CONFIG_HOME=/@XDG@ ${pkgs.lib.getExe pkgs.carapace} _carapace zsh \
          | sed 's|/@XDG@|''${XDG_CONFIG_HOME:-$HOME/.config}|' > snippet

        # Fails the build if carapace stops emitting this line.
        grep '^compdef _carapace_completer ' snippet > registration
        grep -v '^compdef _carapace_completer ' snippet > $out

        tr ' ' '\n' < registration | tail -n +3 | tr -d '"' | sort -u > claimed

        for f in ${
          lib.concatStringsSep " " (lib.unique (map (p: "${p}/share/zsh/site-functions/_*") packages))
        }; do
          f=''${f##*/}
          printf '%s\n' "''${f#_}"
        done | sort -u > shipped

        {
          printf 'compdef _carapace_completer'
          comm -23 claimed shipped | sed 's/.*/ "&"/' | tr -d '\n'
          echo
        } >> $out
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
