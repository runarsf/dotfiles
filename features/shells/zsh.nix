{
  self,
  inputs,
  lib',
  ...
}: let
  mkZshPackage = {
    pkgs,
    system,
    fzfTab ? true,
  }: let
    inherit
      (pkgs.lib)
      getExe
      optional
      makeBinPath
      concatMapStringsSep
      concatMapAttrsStringSep
      ;
    inherit
      (lib'.shell)
      mkZoxideInit
      mkStarshipInit
      greeting
      aliases
      abbrs
      ;

    aliases' =
      (aliases pkgs)
      // {
        ls = "EZA_ICON_SPACING=2 ${getExe pkgs.eza} -l -F -g -a --group-directories-first --no-time --git";
        develop = "nix develop --command zsh";
      };

    eagerPlugins =
      [
        "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
        "${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh"
        "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/init.zsh"
        "${pkgs.zsh-abbr}/share/zsh/zsh-abbr/zsh-abbr.plugin.zsh"
        "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/dotenv/dotenv.plugin.zsh"
        "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh"
        "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/per-directory-history/per-directory-history.plugin.zsh"
        "${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/key-bindings.zsh"
        "${pkgs.fzf}/share/fzf/completion.zsh"
        "${pkgs.fzf}/share/fzf/key-bindings.zsh"
      ]
      ++ optional fzfTab "${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh"
      ++ ["${self.packages.${system}.zsh-docker-aliases}/zsh-docker-aliases.plugin.zsh"];

    # "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
    deferredPlugins = [
      "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
      "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
    ];

    runtimeDeps = makeBinPath (
      with pkgs; [
        carapace
        zoxide
        deja
      ]
      ++ optional fzfTab pkgs.fzf
    );

    configured = inputs.wrapper-modules.wrappers.zsh.wrap {
      inherit pkgs;

      zshenv.content = ''
        AUTOPAIR_INHIBIT_INIT=1
        AUTOPAIR_SPC_WIDGET="abbr-expand-and-insert"
      '';

      zshrc.content =
        ''
          # Workaround for non-nixos machines that can't set pathsToLink
          if (( ! ''${fpath[(I)/run/current-system/sw/share/zsh/site-functions]} )); then
            for p in ''${(z)NIX_PROFILES}; do
              fpath=($p/share/zsh/site-functions $p/share/zsh/''${ZSH_VERSION}/functions $fpath)
            done
          fi

          fpath=(''${XDG_DATA_HOME:-$HOME/.local/share}/zsh/generated_man_completions $fpath)

          zmodload zsh/complist \
                   zsh/mathfunc
          autoload -Uz compinit \
                       vcs_info \
                       edit-command-line \
                       up-line-or-beginning-search \
                       down-line-or-beginning-search
          if [[ -n ''${ZDOTDIR:-$HOME}/.zcompdump(#qN.mh+24) ]]; then
            compinit
          else
            compinit -C
          fi

          bindkey -e

          HISTFILE="''${XDG_STATE_HOME:-$HOME/.local/state}/zsh/history"
          HISTSIZE=50000
          SAVEHIST=50000
          mkdir -p "''${HISTFILE:h}"

          source ${pkgs.zsh-defer}/share/zsh-defer/zsh-defer.plugin.zsh
        ''
        # if [[ ! -d ''${XDG_DATA_HOME:-$HOME/.local/share}/zsh/generated_man_completions ]]; then
        #  zsh-defer ${getExe self.packages.${system}.zsh-manpage-completion-generator}
        #fi
        + ''
          source <(${getExe pkgs.carapace} _carapace zsh)

          ${eagerPlugins |> concatMapStringsSep "\n" (p: "source ${p}")}

          typeset -A ZSH_HIGHLIGHT_REGEXP
          ZSH_HIGHLIGHT_REGEXP+=('[0-9]' fg=cyan)
          ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main regexp)

          ${deferredPlugins |> concatMapStringsSep "\n" (p: "zsh-defer source ${p}")}
          zsh-defer bindkey '^[[A' history-substring-search-up
          zsh-defer bindkey '^[[B' history-substring-search-down

          ${abbrs |> concatMapAttrsStringSep "\n" (k: v: "abbr add --quiet '${k}=${v}'")}
          ${aliases' |> concatMapAttrsStringSep "\n" (k: v: "alias ${k}='${v}'")}

          source ${mkStarshipInit pkgs "zsh"}
          source ${mkZoxideInit pkgs "zsh"}

          vcs_info 'prompt'

          setopt hist_ignore_all_dups \
                 hist_expire_dups_first \
                 hist_ignore_dups \
                 hist_ignore_space \
                 share_history \
                 inc_append_history \
                 extended_history \
                 menu_complete \
                 auto_param_slash \
                 interactive_comments \
                 auto_cd \
                 auto_pushd \
                 pushd_ignore_dups \
                 pushd_silent \
                 hup \
                 long_list_jobs \
                 notify

          unsetopt nomatch \
                   beep \
                   correct \
                   prompt_cr \
                   prompt_sp

          # zstyle ':completion:*' format $'\e[2;37mCompleting %d\e[m'
          zstyle ':completion:*' completer _complete _files
          zstyle ':completion:*' special-dirs ..
          zstyle ':completion:*' special-dirs last
          zstyle ':completion:*' squeeze-slashes true
          zstyle ':completion:*' complete-options true
          zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
          _comp_options+=(globdots)
          ${
            if fzfTab
            then ''
              zstyle ':fzf-tab:*' fzf-flags '--border=rounded' '--height=~50%' '--min-height=20'
              zstyle ':fzf-tab:*' switch-group ',' '.'
            ''
            else ''
              zstyle ':completion:*' menu select
              zstyle ':completion:*:default' list-colors ''${(s.:.)LS_COLORS}
            ''
          }

          if [[ -r "$HOME/.local/share/deja/init.zsh" ]]; then
            source "$HOME/.local/share/deja/init.zsh"
          else
            eval "$(deja init zsh)"
          fi
          export DEJA_EMPTY=off

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

          ze () { "$EDITOR" "$(${getExe pkgs.zoxide} query "$@")" }
          zd () { cd "$(${getExe pkgs.zoxide} query "$PWD" "$@")" }
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
        '';
    };
  in
    (pkgs.symlinkJoin {
      name = "zsh";
      paths = [configured];
      nativeBuildInputs = [pkgs.makeWrapper];
      postBuild = ''
        wrapProgram $out/bin/zsh --prefix PATH : "${runtimeDeps}"
      '';
    })
    // (configured.passthru or {});
in {
  flake.nixosModules.zsh = {
    pkgs,
    lib,
    config,
    ...
  }: let
    inherit (pkgs.stdenv.hostPlatform) system;
    cfg = config.features.zsh;
    pkg = mkZshPackage {
      inherit pkgs system;
      fzfTab = cfg.fzfTab;
    };
  in {
    options.features.zsh.fzfTab =
      lib.mkEnableOption "fzf-tab completion UI"
      // {
        default = false;
      };

    config = {
      programs.command-not-found.enable = false;
      environment.pathsToLink = ["/share/zsh"];
      environment.shells = [pkg];
      environment.systemPackages =
        [
          pkg
          pkgs.carapace
        ]
        ++ lib.optional cfg.fzfTab pkgs.fzf;
    };
  };

  perSystem = {
    pkgs,
    system,
    ...
  }: {
    packages.zsh = mkZshPackage {inherit pkgs system;};
  };
}
