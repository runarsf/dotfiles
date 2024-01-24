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

  programs.zsh = {
    enable = true;

    enableAutosuggestions = true;
    enableCompletion = true;
    syntaxHighlighting.enable = true;

    antidote = {
      enable = true;
      useFriendlyNames = true;
      plugins = [
        "zsh-users/zsh-autosuggestions"
        "zsh-users/zsh-syntax-highlighting"
        "zsh-users/zsh-completions"
        "hlissner/zsh-autopair"
        "zsh-users/zsh-history-substring-search"
        "akarzim/zsh-docker-aliases"
        "jimhester/per-directory-history"
        "z-shell/zsh-diff-so-fancy"
        "nix-community/nix-zsh-completions"
        "MichaelAquilina/zsh-auto-notify"
        "olets/zsh-abbr"
        "zshzoo/magic-enter"
        "agkozak/zsh-z"
        "ohmyzsh/ohmyzsh path:lib"
        "ohmyzsh/ohmyzsh path:plugins/extract"
      ];
    };
    initExtra = ''
      # https://thevaluable.dev/zsh-completion-guide-examples/
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

      abbr -S -qq yay=paru
      bindkey " " abbr-expand-and-space
      AUTO_NOTIFY_IGNORE+=("zsh" "lf")

      wim () { ''${EDITOR} "$(which ''${1:?No file selected...})" "''${@:2}" }
      magic-enter-cmd () {
        if command git rev-parse --is-inside-work-tree &>/dev/null; then
           printf 'git -c color.ui=always status -sb --show-stash --ignore-submodules'
        else
           printf '${pkgs.eza}/bin/eza -laF --git --no-time --group-directories-first --hyperlink'
        fi
      }

      # Workaround for pyprland overriding the python version with its bundled one
      export PATH="''${HOME}/.nix-profile/bin:''${PATH}";
    '';
    prezto = {
      enable = true;
      caseSensitive = false;
      terminal.autoTitle = true;
      editor.dotExpansion = true;
      editor.promptContext = true;
      utility.safeOps = false;
    };
  };
}
