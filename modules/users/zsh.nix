{ config, outputs, pkgs, ... }:

{
  imports = [
    ../../modules/users/starship.nix
    ../../modules/users/shell-utils.nix
  ];
} // outputs.lib.mkModule config "Zsh" {
  modules.starship.enable = true;

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
        "zshzoo/magic-enter"
        "jimhester/per-directory-history"
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
    initExtraFirst = ''
      AUTOPAIR_INHIBIT_INIT=1
      AUTOPAIR_SPC_WIDGET="abbr-expand-and-insert"
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
      # FIXME nonomatch

      _comp_options+=(globdots)

      wim () { set -eu; ''${EDITOR:-vim} "$(which ''${1:?No file selected...})" ''${@:2}; set +eu }
      magic-enter-cmd () {
        print ' ${pkgs.krabby}/bin/krabby random | tail -n+2'
      }

      __git_files () {
        _wanted files expl 'local files' _files
      }

      lf () { cd "$(${pkgs.walk}/bin/walk "$@")" }
      ze () { "$EDITOR" "$("${config.programs.zoxide.package}/bin/zoxide" query "$@")" }
      zcode () { "${config.programs.vscode.package}/bin/code" "$("${config.programs.zoxide.package}/bin/zoxide" query "$@")" }
      zd () { set -e; cd "$("${config.programs.zoxide.package}/bin/zoxide" query "$PWD" "$@")"; set +e }

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
