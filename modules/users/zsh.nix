{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config "zsh" {
  nixpkgs.overlays = [
    (_: prev: {
      stderred = prev.stderred.overrideAttrs {
        src = pkgs.fetchFromGitHub {
          owner = "sickill";
          repo = "stderred";
          rev = "97a51c97f757d3a23efc90b15bf95d5af632c8f8";
          sha256 = "sha256-aXpn1H5SvpP5XufBaoiI38S+jUXOH+f65ntvbYkLrrQ=";
        };
      };
    })
  ];
  nixos.programs.command-not-found.enable = false;
  programs.command-not-found.enable = false;
  programs.nix-index.enable = false;

  modules.starship.enable = true;

  home = {
    packages = with pkgs; [
      libnotify
      libqalculate
    ];
    shellAliases = {
      develop = outputs.lib.mkForce "nix develop --command zsh";
    };
  };

  nixos.environment.pathsToLink = ["/share/zsh"];

  programs = {
    zoxide.enable = true;
    fzf.enable = true;

    zsh = {
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
          "Tarrasch/zsh-functional"
          "zshzoo/magic-enter"
          "MichaelAquilina/zsh-you-should-use"
          # "arzzen/calc.plugin.zsh"

          "getantidote/use-omz"
          "ohmyzsh/ohmyzsh path:lib"
          "ohmyzsh/ohmyzsh path:plugins/extract"
          "ohmyzsh/ohmyzsh path:plugins/gitfast"
          "ohmyzsh/ohmyzsh path:plugins/dotenv"
          "ohmyzsh/ohmyzsh path:plugins/fancy-ctrl-z"
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

        # TODO Make nix-files autocomplete before lock-files
        # zstyle ':completion:*:default' file-patterns \
        #   '*.(#i)nix:nix-files' \
        #   '*.(#i)lock:lock-files' \
        #   '%p:all-files'
        # zstyle ':completion:*:default' group-order nix-files lock-files all-files

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

        # https://unix.stackexchange.com/a/26776
        # https://unix.stackexchange.com/a/53587
        export STDERRED_BLACKLIST="^(niks|nix|nh|ssh|gitui|vim|neovim|just|yazi)$"
        export LD_PRELOAD="${pkgs.stderred}/lib/libstderred.so''${LD_PRELOAD:+:$LD_PRELOAD}"

        wim () { set -eu; ''${EDITOR:-vim} "$(which ''${1:?No file selected...})" ''${@:2}; set +eu }
        ? () {
          ${pkgs.krabby}/bin/krabby random --no-title
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
        alias docker-compose='docker compose'
        alias dkcUf='docker compose up -d --force-recreate'

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
  };
}
