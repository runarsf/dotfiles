{ outputs, pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      libnotify
    ];
    shellAliases = {
      develop = outputs.lib.mkForce "nix develop --command zsh";
    };
  };

  programs.zoxide.enable = true;
  programs.fzf.enable = true;

  nixos.environment.pathsToLink = [ "/share/zsh" ];

  programs.zsh = {
    enable = true;

    antidote = {
      enable = true;
      useFriendlyNames = true;
      plugins = [
        "hlissner/zsh-autopair"
        "akarzim/zsh-docker-aliases"
        "z-shell/zsh-diff-so-fancy"
        "nix-community/nix-zsh-completions"
        "zshzoo/magic-enter"
        "jimhester/per-directory-history"
        "ohmyzsh/ohmyzsh path:lib"
        "ohmyzsh/ohmyzsh path:plugins/extract"
        # "zsh-users/zsh-syntax-highlighting"
        # "zsh-users/zsh-autosuggestions"
        # "zsh-users/zsh-completions"
        # "zsh-users/zsh-history-substring-search"
        # "darvid/zsh-poetry"
      ];
    };
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
      # FIXME nonomatch

      _comp_options+=(globdots)

      wim () { set -o nounset; set -o errexit; ''${EDITOR} "$(which ''${1:?No file selected...})" ''${@:2} }
      magic-enter-cmd () {
        # if command git rev-parse --is-inside-work-tree &>/dev/null; then
        #    printf 'git -c color.ui=always status -sb --show-stash --ignore-submodules'
        # else
        #    printf 'ls'
        # fi
        print '${pkgs.krabby}/bin/krabby random | tail -n+2'
      }
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
