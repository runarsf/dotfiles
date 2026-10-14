{
  self,
  inputs,
  lib',
  ...
}:
let
  # Identity used to evaluate `homeModules.zsh` on its own for `packages.zsh`.
  # The generated .zshrc has to work under any $HOME, so every runtime path in
  # the module below is spelled with shell expansions ($HOME, $XDG_*) instead of
  # `config.home.homeDirectory`, and `programs.zsh.plugins` is avoided (it
  # symlinks plugins into the home directory and sources them from there).
  portableHome = {
    home = {
      username = "portable";
      homeDirectory = "/home/portable";
      stateVersion = "24.11";
    };
  };
in
{
  flake.homeModules.zsh =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      inherit (lib)
        concatMapAttrsStringSep
        concatMapStringsSep
        escapeShellArg
        mkDefault
        mkEnableOption
        mkMerge
        mkOrder
        optional
        optionalString
        ;
      inherit (lib'.shell)
        mkCarapaceInit
        mkDirenvHook
        mkStarshipInit
        mkZoxideInit
        greeting
        aliases
        abbrs
        ;

      cfg = config.features.zsh;

      zshCacheHome = "\${XDG_CACHE_HOME:-$HOME/.cache}/zsh";
      zcompdump = "${zshCacheHome}/zcompdump";
      omzCacheDir = "\${XDG_CACHE_HOME:-$HOME/.cache}/oh-my-zsh";
      manCompletions = "\${XDG_DATA_HOME:-$HOME/.local/share}/zsh/generated_man_completions";

      # zsh-abbr reads this file on startup and runs every `abbr` line in it,
      # which is the same work as calling `abbr add` per abbreviation, except
      # declarative. Store path, so `abbr add` at runtime will fail; use
      # `abbr add -S` for session-scoped ones.
      userAbbreviations = pkgs.writeText "zsh-user-abbreviations" (
        concatMapAttrsStringSep "\n" (k: v: "abbr ${escapeShellArg k}=${escapeShellArg v}") abbrs + "\n"
      );

      aliases' = aliases pkgs // {
        develop = "nix develop --command zsh";
        zj = "zellij";
      };

      # Sourced eagerly, because the block at order 1000 binds keys to widgets
      # and completers these define.
      eagerPlugins =
        [
          "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
          "${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh"
          "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/init.zsh"
          "${pkgs.zsh-abbr}/share/zsh/zsh-abbr/zsh-abbr.plugin.zsh"
          "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh"
          "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/per-directory-history/per-directory-history.plugin.zsh"
          "${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/key-bindings.zsh"
          "${config.programs.fzf.package}/share/fzf/completion.zsh"
          "${config.programs.fzf.package}/share/fzf/key-bindings.zsh"
        ]
        ++ optional cfg.fzfTab "${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh"
        ++ [ "${self.packages.${system}.zsh-docker-aliases}/zsh-docker-aliases.plugin.zsh" ];

      # Nothing before the first prompt needs these, so zsh-defer them.
      deferredPlugins = [
        "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
        "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
        "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
      ];
    in
    {
      options.features.zsh.fzfTab = mkEnableOption "fzf-tab completion UI";

      config = {
        programs = {
          # Each of these integrations is an `eval "$(foo init zsh)"` in
          # home-manager, i.e. a process spawn per startup (~23ms for the four
          # below, ~5ms more for direnv). The snippets are static, so they are
          # generated at build time and sourced from the store instead; the
          # modules stay in charge of packages and configuration. mkDefault so a
          # host can opt back in.
          starship.enableZshIntegration = mkDefault false;
          zoxide.enableZshIntegration = mkDefault false;
          carapace.enableZshIntegration = mkDefault false;
          fzf.enableZshIntegration = mkDefault false;
          direnv.enableZshIntegration = mkDefault false;

          # Hard dependencies of the .zshrc below, which sources all four
          # unconditionally: fzf's completion.zsh and key-bindings.zsh come
          # straight out of its package, and the generated zoxide/carapace inits
          # call their binaries by name, so those two have to be on PATH.
          starship.enable = true;
          zoxide.enable = true;
          carapace.enable = true;
          fzf.enable = true;

          zsh = {
            enable = true;
            dotDir = "${config.xdg.configHome}/zsh";

            autocd = true;
            defaultKeymap = "emacs";

            # All three are sourced through zsh-defer in the mkAfter block
            # instead of eagerly where home-manager would put them.
            autosuggestion.enable = false;
            syntaxHighlighting.enable = false;
            historySubstringSearch.enable = false;

            history = {
              path = "\${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history";
              size = 50000;
              save = 50000;
              extended = true;
              expireDuplicatesFirst = true;
              ignoreAllDups = true;
              ignoreDups = true;
              ignoreSpace = true;
              share = true;
            };

            # The HIST_* options and AUTO_CD come from the `history` and
            # `autocd` options above.
            setOptions = [
              "INC_APPEND_HISTORY"
              "MENU_COMPLETE"
              "AUTO_PARAM_SLASH"
              "INTERACTIVE_COMMENTS"
              "AUTO_PUSHD"
              "PUSHD_IGNORE_DUPS"
              "PUSHD_SILENT"
              "HUP"
              "LONG_LIST_JOBS"
              "NOTIFY"
              "NO_NOMATCH"
              "NO_BEEP"
              "NO_CORRECT"
              "NO_PROMPT_CR"
              "NO_PROMPT_SP"
            ];

            shellAliases = aliases';

            localVariables = {
              AUTOPAIR_INHIBIT_INIT = 1;
              AUTOPAIR_SPC_WIDGET = "abbr-expand-and-insert";
              ABBR_USER_ABBREVIATIONS_FILE = "${userAbbreviations}";
              ZSH_CACHE_DIR = omzCacheDir;
            };

            completionInit = ''
              autoload -Uz compinit

              # A full compinit rescans $fpath and re-checks permissions on every
              # entry; -C skips both and just loads the dump. Only pay for the
              # full run when the dump is missing or over a day old.
              # NOTE: glob qualifiers are not expanded inside [[ ]], hence the
              # array -- `[[ -n $file(#qN.mh+24) ]]` is always true.
              _zcompdump_stale=( ${zcompdump}(Nmh+24) )
              if [[ ! -s "${zcompdump}" || -n $_zcompdump_stale ]]; then
                compinit -d "${zcompdump}"
              else
                compinit -C -d "${zcompdump}"
              fi
              unset _zcompdump_stale
            '';

            initContent = mkMerge [
              # Everything compinit (570) depends on, plus zsh-defer for the
              # mkAfter block. home-manager's order 510/520 chunks already
              # handle `typeset -U fpath` and the $NIX_PROFILES fpath entries.
              (mkOrder 550 ''
                zmodload zsh/complist \
                         zsh/mathfunc

                autoload -Uz vcs_info \
                             edit-command-line \
                             up-line-or-beginning-search \
                             down-line-or-beginning-search

                fpath=(${manCompletions} $fpath)

                mkdir -p "${zshCacheHome}" "${omzCacheDir}"

                source ${pkgs.zsh-defer}/share/zsh-defer/zsh-defer.plugin.zsh
              '')

              # After compinit (570), because fzf-tab and the carapace completer
              # need compdef, and after home-manager's history block (910),
              # because per-directory-history captures $HISTFILE at source time.
              # Still before setOptions (950) and the keybindings below (1000),
              # which is the order the hand-rolled zshrc used.
              (mkOrder 920 ''
                source ${mkCarapaceInit pkgs "zsh"}

                ${concatMapStringsSep "\n" (p: "source ${p}") eagerPlugins}
              '')

              (mkOrder 1000 ''
                # zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
                zstyle ':completion:*' completer _complete _files
                zstyle ':completion:*' special-dirs ..
                zstyle ':completion:*' special-dirs last
                zstyle ':completion:*' squeeze-slashes true
                zstyle ':completion:*' complete-options true
                zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
                _comp_options+=(globdots)
                ${
                  if cfg.fzfTab then
                    ''
                      zstyle ':fzf-tab:*' fzf-flags '--border=rounded' '--height=~50%' '--min-height=20'
                      zstyle ':fzf-tab:*' switch-group ',' '.'
                    ''
                  else
                    ''
                      zstyle ':completion:*' menu select
                      zstyle ':completion:*:default' list-colors ''${(s.:.)LS_COLORS}
                    ''
                }

                magic-enter-cmd () { ${greeting pkgs} }
                magic-enter () {
                  if [[ -z $BUFFER ]]; then
                    magic-enter-cmd
                    zle accept-line
                  else
                    zle accept-line
                  fi
                }
                zle -N magic-enter
                zle -N edit-command-line
                zle -N up-line-or-beginning-search
                zle -N down-line-or-beginning-search
                bindkey "^M" magic-enter
                bindkey '^e' edit-command-line
                bindkey '^G' per-directory-history-toggle-history
                bindkey -M vicmd '^G' per-directory-history-toggle-history

                __git_files () {
                  _wanted files expl 'local files' _files
                }

                ze () { "$EDITOR" "$(${lib.getExe config.programs.zoxide.package} query "$@")" }
                zd () { cd "$(${lib.getExe config.programs.zoxide.package} query "$PWD" "$@")" }
                wim () { set -eu; ''${EDITOR:-vim} "$(which ''${1:?No file selected...})" ''${@:2}; set +eu }
                tmpvim () {
                  if test ! -f "''${1:?No file specified...}"; then
                    printf "File doesn't exist...\n"
                    return 1
                  fi
                  trap "mv '$1.bak' '$1'" EXIT
                  mv "$1" "$1.bak"
                  cat "$1.bak" > "$1"
                  $EDITOR "$1"
                }
                if command -v nvim &>/dev/null; then
                  alias vim='nvim'
                fi

                autopair-init

                vcs_info 'prompt'

                source ${mkStarshipInit pkgs "zsh"}
                source ${mkZoxideInit pkgs "zsh"}
                ${optionalString config.programs.direnv.enable "source ${mkDirenvHook pkgs "zsh"}"}

                # TODO: Zellij part should be set in zellij.nix
                if test -n "$ZELLIJ"; then
                  if test "$ZELLIJ_SESSION_NAME" = "zj-launcher"; then
                    zellij kill-session "$ZELLIJ_SESSION_NAME"
                  elif test -n "$ZJ_FROM_LAUNCHER"; then
                    z "$ZELLIJ_SESSION_NAME" 2>/dev/null
                  fi
                fi
              '')

              # Last, so zsh-syntax-highlighting sees every widget defined above
              # (it wraps the ones that exist when it is sourced). zsh-defer runs
              # these in order, just before the first prompt.
              # NOTE: upstream recommends loading history-substring-search after
              # zsh-syntax-highlighting; this keeps the order the hand-rolled
              # config used. Swap the first two entries of `deferredPlugins` to
              # follow upstream.
              (mkOrder 1500 ''
                typeset -A ZSH_HIGHLIGHT_REGEXP
                ZSH_HIGHLIGHT_REGEXP+=('[0-9]' fg=cyan)
                ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main regexp)

                ${concatMapStringsSep "\n" (p: "zsh-defer source ${p}") deferredPlugins}
                zsh-defer bindkey '^[[A' history-substring-search-up
                zsh-defer bindkey '^[[B' history-substring-search-down
              '')
            ];
          };
        };
      };
    };

  flake.nixosModules.zsh =
    { lib, ... }:
    {
      # Replaced by nix-index-database's handler (see features/nix.nix).
      programs.command-not-found.enable = false;

      # Mainly here for /etc/zshenv -- it is what sets up PATH/NIX_PROFILES for
      # login shells -- and for environment.pathsToLink = [ "/share/zsh" ] and
      # environment.shells. Everything interactive is home-manager's job, so the
      # parts of /etc/zshrc that would duplicate it, fight it or fork a process
      # are turned off. /etc/zshrc still costs one `hostname --fqdn`; add
      # `programs.zsh.envExtra = "setopt no_global_rcs"` to the home module to
      # skip it (and /etc/zprofile) entirely.
      programs.zsh = {
        enable = true;
        enableGlobalCompInit = false;
        enableLsColors = false;
        promptInit = "";
        setOptions = [ ];
        # environment.shellAliases would otherwise land in /etc/zshrc and
        # shadow (or be shadowed by) the home-manager ones.
        shellAliases = lib.mkForce { };
      };
    };

  perSystem =
    {
      pkgs,
      lib,
      ...
    }:
    let
      portable = inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          self.homeModules.zsh
          portableHome
        ];
      };

      # home-manager's own .zshenv/.zprofile are tied to a managed home, so the
      # portable build only reuses the .zshrc and reimplements the one thing
      # those files do that matters elsewhere: sourcing hm-session-vars.sh
      # (EDITOR, GIT_EDITOR, ...).
      zdotdir = pkgs.linkFarm "zsh-zdotdir" {
        ".zshrc" = pkgs.writeText "zshrc" portable.config.programs.zsh.initContent;
        ".zshenv" = pkgs.writeText "zshenv" ''
          for p in ''${(z)NIX_PROFILES}; do
            if [[ -r "$p/etc/profile.d/hm-session-vars.sh" ]]; then
              source "$p/etc/profile.d/hm-session-vars.sh"
            fi
          done
        '';
      };

      runtimeDeps = lib.makeBinPath (
        with pkgs;
        [
          carapace
          zoxide
          fzf
        ]
      );
    in
    {
      packages.zsh = pkgs.symlinkJoin {
        name = "zsh";
        paths = [ pkgs.zsh ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/zsh \
            --set ZDOTDIR "${zdotdir}" \
            --prefix PATH : "${runtimeDeps}"
        '';
        meta.mainProgram = "zsh";
      };
    };
}
