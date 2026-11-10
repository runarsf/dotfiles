{
  self,
  lib',
  ...
}:
{
  flake.homeModules.zsh =
    {
      config,
      pkgs,
      lib,
      # Absent for the standalone homeConfigurations; set when home-manager runs
      # as a NixOS module, which is where the system's man pages come from.
      osConfig ? { },
      ...
    }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      inherit (lib)
        concatMapAttrsStringSep
        concatMapStringsSep
        escapeShellArg
        mkAfter
        mkDefault
        mkMerge
        mkOrder
        optional
        optionalString
        substring
        getExe
        ;
      inherit (lib'.shell)
        mkCarapaceZshInit
        mkDirenvHook
        mkLsColors
        mkDocCompletions
        mkStarshipInit
        mkZoxideInit
        greeting
        aliases
        abbrs
        ;

      cfg = config.features.zsh;

      zshCacheHome = "\${XDG_CACHE_HOME:-$HOME/.cache}/zsh";
      omzCacheDir = "\${XDG_CACHE_HOME:-$HOME/.cache}/oh-my-zsh";
      packages = config.home.packages ++ (osConfig.environment.systemPackages or [ ]);
      docCompletions = mkDocCompletions {
        inherit pkgs packages;
        generator = self.packages.${system}.zsh-manpage-completion-generator;
        # Nothing else completes these; `--help` is all they have.
        commands = [
          "alejandra"
          "blahaj"
          "cava"
          "claude"
          "deadnix"
          "matugen"
          "niks"
          "oxker"
          "podman-compose"
          "spd-say"
          "sqlit"
          "tomlq"
        ];
      };
      carapaceInit = mkCarapaceZshInit { inherit pkgs packages; };

      # Keyed on the generated completions, so regenerating them takes effect
      # at once instead of waiting out the staleness check below.
      zcompdump = "${zshCacheHome}/zcompdump-${substring 0 8 (baseNameOf docCompletions)}";

      omz = "${pkgs.oh-my-zsh}/share/oh-my-zsh";

      userAbbreviations = pkgs.writeText "zsh-user-abbreviations" (
        concatMapAttrsStringSep "\n" (k: v: "abbr ${escapeShellArg k}=${escapeShellArg v}") abbrs + "\n"
      );

      aliases' = aliases pkgs // {
        develop = "nix develop --command zsh";
        zj = "zellij";
      };

      sourceAll = concatMapStringsSep "\n" (p: "source ${p}");
      deferAll = concatMapStringsSep "\n" (p: "zsh-defer source ${p}");

      # home-manager's slots: compinit 570, history 910.
      beforeCompinit = mkOrder 550;
      afterHistory = mkOrder 920;

      eagerPlugins = [
        "${self.packages.${system}.zsh-ez-compinit}/ez-compinit.plugin.zsh"
        "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
        "${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh"
        "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/init.zsh"
        "${pkgs.zsh-abbr}/share/zsh/zsh-abbr/zsh-abbr.plugin.zsh"
        "${omz}/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh"
        "${omz}/plugins/per-directory-history/per-directory-history.plugin.zsh"
        "${omz}/lib/key-bindings.zsh"
        "${config.programs.fzf.package}/share/fzf/completion.zsh"
        "${config.programs.fzf.package}/share/fzf/key-bindings.zsh"
        "${self.packages.${system}.zsh-docker-aliases}/zsh-docker-aliases.plugin.zsh"
        "${self.packages.${system}.zsh-history-on-success}/zsh-history-on-success.plugin.zsh"
      ]
      ++ optional cfg.fzfTab "${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh";

      deferredPlugins = [
        "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
        "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
        "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
        "${self.packages.${system}.zsh-fz}/fz.plugin.zsh"
      ];
    in
    {
      options.features.zsh = {
        fzfTab = lib.mkEnableOption "fzf-tab completion UI";
      };

      config = {
        programs = {
          # Disable shell integrations that run on startup
          starship.enableZshIntegration = mkDefault false;
          zoxide.enableZshIntegration = mkDefault false;
          carapace.enableZshIntegration = mkDefault false;
          fzf.enableZshIntegration = mkDefault false;
          direnv.enableZshIntegration = mkDefault false;
          television.enableZshIntegration = mkDefault false;
          nix-index.enableZshIntegration = mkDefault false;

          starship.enable = true;
          zoxide.enable = true;
          carapace.enable = true;
          fzf.enable = true;

          zsh = {
            enable = true;
            dotDir = "${config.xdg.configHome}/zsh";

            autocd = true;
            defaultKeymap = "emacs";

            # Deferred at the end of the file instead.
            autosuggestion.enable = false;
            syntaxHighlighting.enable = false;
            historySubstringSearch.enable = false;

            envExtra = "setopt no_global_rcs"; # Skip `hostname --fqdn` and /etc/zprofile

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

            # HIST_* and AUTO_CD come from the options above.
            setOptions = [
              "INC_APPEND_HISTORY"
              # Tab goes straight into the highlighted `menu select` list.
              "MENU_COMPLETE"
              "COMPLETE_IN_WORD"
              "ALWAYS_TO_END"
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
              # fz.sh defaults to rupa/z; we have zoxide.
              FZ_HISTORY_CD_CMD = "__zoxide_z";
              FZ_HISTORY_LIST_GENERATOR = "__fz_zoxide_list";
            };

            completionInit = ''
              autoload -Uz compinit

              # -C skips the $fpath rescan. The dump's name covers every package
              # that contributes to $fpath, so a full run is only needed when that
              # name changes; the old dumps go with it.
              if [[ -s "${zcompdump}" ]]; then
                compinit -C -d "${zcompdump}"
              else
                compinit -d "${zcompdump}"
                for _d in ${zshCacheHome}/zcompdump-*(N); do
                  [[ $_d == "${zcompdump}" ]] || rm -f -- "$_d"
                done
                unset _d
              fi
            '';

            initContent = mkMerge [
              (beforeCompinit ''
                zmodload zsh/complist

                autoload -Uz vcs_info \
                             edit-command-line \
                             up-line-or-beginning-search \
                             down-line-or-beginning-search

                # Appended, so hand-written completers keep precedence.
                fpath+=(${docCompletions}/share/zsh/site-functions)

                mkdir -p "${zshCacheHome}" "${omzCacheDir}"

                source ${mkLsColors pkgs}
                source ${pkgs.zsh-defer}/share/zsh-defer/zsh-defer.plugin.zsh
              '')

              # After compinit for compdef, and after the history block --
              # per-directory-history reads $HISTFILE when sourced.
              (afterHistory ''
                source ${carapaceInit}

                ${sourceAll eagerPlugins}
              '')

              ''
                # Real completer first, plain filenames as the fallback.
                zstyle ':completion:*' completer _complete _files

                # Exact prefix, then case-insensitive, then anywhere in the word.
                zstyle ':completion:*' matcher-list "" 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'l:|=* r:|=*'

                # Offer .. when completing a path.
                zstyle ':completion:*' special-dirs ..

                # Treat foo//bar as foo/bar.
                zstyle ':completion:*' squeeze-slashes true

                # Let cd offer its options too; nothing else reads this.
                zstyle ':completion:*' complete-options true

                # Flags on a bare Tab, above the files. The tag-order is what makes
                # prefix-needed take effect -- _files clobbers the tag list otherwise.
                # zstyle ':completion:*' prefix-needed false
                # zstyle ':completion:*' tag-order 'options'

                # Directories above files.
                zstyle ':completion:*' list-dirs-first true

                # Show the description a completer gives each match.
                zstyle ':completion:*' verbose yes

                # Split matches into groups by kind...
                zstyle ':completion:*' group-name ""
                # ...with a dim header over each.
                zstyle ':completion:*:descriptions' format '%F{244}%d%f'

                # Keep results from completers that are slow to ask.
                zstyle ':completion:*' use-cache on
                zstyle ':completion:*' cache-path "${zshCacheHome}/zcompcache"

                # Colour the pid and the name in `kill <Tab>`.
                zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

                # Match dotfiles without typing the dot.
                _comp_options+=(globdots)

                # File colours in every group, not just the ungrouped `default` one.
                zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}

                ${
                  if cfg.fzfTab then
                    ''
                      zstyle ':completion:*' menu no

                      zstyle ':fzf-tab:*' use-fzf-default-opts yes
                      zstyle ':fzf-tab:*' fzf-flags '--height=~50%' '--min-height=20' '--border=none'
                      zstyle ':fzf-tab:*' switch-group 'left' 'right'

                      zstyle ':fzf-tab:complete:cd:*' fzf-preview '${getExe pkgs.eza} -1 --color=always $realpath'

                      # fzf-tab reuses the group heading verbatim, so no %F codes here.
                      zstyle ':completion:*:descriptions' format '[%d]'

                      # Heading even when there is just one group.
                      zstyle ':fzf-tab:*' single-group color header

                      # carapace shows basenames but completes full paths, so the typed
                      # path would end up in the query and match none of them.
                      zstyle ':fzf-tab:*' query-string prefix
                    ''
                  else
                    ''
                      # Arrow-key menu, current match highlighted.
                      zstyle ':completion:*' menu select

                      # Where you are while scrolling a long menu.
                      zstyle ':completion:*' select-prompt '%S%p%s'
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
                # Handled by history-substring-search-*
                # zle -N up-line-or-beginning-search
                # zle -N down-line-or-beginning-search
                bindkey "^M" magic-enter
                bindkey '^e' edit-command-line
                bindkey '^G' per-directory-history-toggle-history
                bindkey -M vicmd '^G' per-directory-history-toggle-history

                # Only matters when `compdef _git git` is set, but carapace is used.
                # __git_files () {
                #   _wanted files expl 'local files' _files
                # }

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
                # What fz.sh lists on Tab after `z`.
                __fz_zoxide_list () { zoxide query -l -- "$@" 2>/dev/null }
                ${optionalString config.programs.direnv.enable "source ${mkDirenvHook pkgs "zsh"}"}
              ''

              # Last, so syntax highlighting wraps every widget defined above.
              (mkAfter ''
                typeset -A ZSH_HIGHLIGHT_REGEXP
                ZSH_HIGHLIGHT_REGEXP+=('[0-9]' fg=cyan)
                ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main regexp)

                bindkey '^R' fzf-history-widget

                ${deferAll deferredPlugins}
                zsh-defer bindkey '^[[A' history-substring-search-up
                zsh-defer bindkey '^[[B' history-substring-search-down
              '')
            ];
          };
        };
      };
    };

  flake.nixosModules.zsh = { lib, ... }: {
    programs.command-not-found.enable = false;

    # For /etc/zshenv (PATH/NIX_PROFILES), /share/zsh in pathsToLink,
    # and environment.shells.
    programs.zsh = {
      enable = true;
      enableGlobalCompInit = false;
      enableLsColors = false;
      promptInit = "";
      setOptions = [ ];
      # Would otherwise land in /etc/zshrc and fight the home-manager ones.
      shellAliases = lib.mkForce { };
    };
  };
}
