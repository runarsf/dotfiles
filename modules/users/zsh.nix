{ config, outputs, pkgs, ... }:

outputs.lib.mkModule config "zsh" {
  modules.starship.enable = true;

  home = {
    packages = with pkgs; [ libnotify libqalculate ];
    shellAliases = {
      develop = outputs.lib.mkForce "nix develop --command zsh";
    };
  };

  programs.zoxide.enable = true;
  programs.fzf.enable = true;

  nixos.environment.pathsToLink = [ "/share/zsh" ];

  programs.zsh = {
    enable = true;

    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;

    antidote = {
      enable = true;
      useFriendlyNames = true;
      plugins = [
        "hlissner/zsh-autopair"
        "akarzim/zsh-docker-aliases"
        "z-shell/zsh-diff-so-fancy"
        "nix-community/nix-zsh-completions"
        "MichaelAquilina/zsh-auto-notify"
        "jimhester/per-directory-history"
        "Sam-programs/zsh-calc"
        "Tarrasch/zsh-functional"
        "zshzoo/magic-enter"
        "getantidote/use-omz"
        "ohmyzsh/ohmyzsh path:lib"
        "ohmyzsh/ohmyzsh path:plugins/extract"
      ];
    };
    zsh-abbr = {
      enable = true;
      abbreviations = {
        yay = "paru";
        nh = "niks";
      };
    };
    # FIXME abbrs don't work
    initExtraFirst = ''
      AUTOPAIR_INHIBIT_INIT=1
      # AUTOPAIR_SPC_WIDGET="abbr-expand-and-insert"
      AUTO_NOTIFY_THRESHOLD=20
      AUTO_NOTIFY_EXPIRE_TIME=20000
    '';
    initExtraBeforeCompInit = ''
      autoload -Uz vcs_info
      vcs_info 'prompt'
    '';
    initExtra = ''
      zstyle ':completion:*' special-dirs ..
      zstyle ':completion:*' special-dirs last
      zstyle ':completion:*' squeeze-slashes true
      zstyle ':completion:*' complete-options true

      unsetopt correct \
               prompt_cr \
               prompt_sp

      setopt histignorealldups \
             sharehistory \
             menucomplete \
             autoparamslash \
             nonomatch

      _comp_options+=(globdots)

      typeset -A ZSH_HIGHLIGHT_REGEXP
      ZSH_HIGHLIGHT_REGEXP+=('[0-9]' fg=cyan)
      ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main regexp)

      wim () { set -eu; ''${EDITOR:-vim} "$(which ''${1:?No file selected...})" ''${@:2}; set +eu }
      ? () {
        ${pkgs.krabby}/bin/krabby random | tail -n+2
      }
      magic-enter-cmd () {
        printf ' ?\n'
      }

      __git_files () {
        _wanted files expl 'local files' _files
      }

      ze () { "$EDITOR" "$("${config.programs.zoxide.package}/bin/zoxide" query "$@")" }
      zcode () { "${config.programs.vscode.package}/bin/code" "$("${config.programs.zoxide.package}/bin/zoxide" query "$@")" }
      zd () { set -e; cd "$("${config.programs.zoxide.package}/bin/zoxide" query "$PWD" "$@")"; set +e }
      electron-wayland () { "''${1:?No program specificed...}" --enable-features=UseOzonePlatform,WaylandWindowDecorations --platform-hint=auto --ozone-platform=wayland "''${@:2}" }
      tmpvim () {
        if test ! -f "''${1:?No file specified...}"; then
          printf "File doesn't exist...\n"
        fi

        mv "$1" "$1.bak"
        cat "$1.bak" > "$1"
        $EDITOR "$1"
        mv "$1.bak" "$1"
      }

      bindkey '^G' per-directory-history-toggle-history
      bindkey -M vicmd '^G' per-directory-history-toggle-history

      autopair-init
    '';
    prezto = {
      enable = true;
      caseSensitive = false;
      terminal.autoTitle = true;
      editor.dotExpansion = true;
      editor.promptContext = true;
      utility.safeOps = false;
      pmodules = [
        "ssh"
        "environment"
        "terminal"
        "history"
        "directory"
        "spectrum"
        "utility"
        "syntax-highlighting"
        "autosuggestions"
        "completion"
      ];
    };
  };
}
