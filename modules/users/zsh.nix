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

  modules.starship.enable = true;

  home.packages = with pkgs; [
    libnotify
    libqalculate
  ];

  nixos.environment.pathsToLink = [ "/share/zsh" ];

  nixos.programs.command-not-found.enable = false;
  programs = {
    command-not-found.enable = false;
    nix-index.enable = false;
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
          "Tarrasch/zsh-functional"
          "zshzoo/magic-enter"
          "MichaelAquilina/zsh-you-should-use"
          # "arzzen/calc.plugin.zsh"

          "getantidote/use-omz"
          "ohmyzsh/ohmyzsh path:lib"
          "ohmyzsh/ohmyzsh path:plugins/ssh-agent"
          "ohmyzsh/ohmyzsh path:plugins/extract"
          "ohmyzsh/ohmyzsh path:plugins/gitfast"
          "ohmyzsh/ohmyzsh path:plugins/dotenv"
          "ohmyzsh/ohmyzsh path:plugins/fancy-ctrl-z"
          "ohmyzsh/ohmyzsh path:plugins/extract"
          "ohmyzsh/ohmyzsh path:plugins/per-directory-history"
        ];
      };
      zsh-abbr = {
        enable = true;
        abbreviations = {
          yay = "paru";
          nh = "niks";
        };
      };
      initContent =
        let
          zshConfigEarlyInit = outputs.lib.mkOrder 500 ''
            AUTOPAIR_INHIBIT_INIT=1
            AUTOPAIR_SPC_WIDGET="abbr-expand-and-insert"
            AUTO_NOTIFY_THRESHOLD=20
            AUTO_NOTIFY_EXPIRE_TIME=20000
            zstyle :omz:plugins:ssh-agent identities ${
              config.modules.ssh.keys |> builtins.attrNames |> builtins.concatStringsSep " "
            }
            zstyle :omz:plugins:ssh-agent lazy yes
            zstyle :omz:plugins:ssh-agent agent-forwarding yes
            zstyle :omz:plugins:ssh-agent quiet yes
          '';
          zshConfigBeforeCompInit = outputs.lib.mkOrder 550 ''
            autoload -Uz vcs_info
            vcs_info 'prompt'
          '';
          zshConfig = outputs.lib.mkOrder 1000 ''
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
            # export STDERRED_BLACKLIST="^(niks|nix|nh|ssh|gitui|vim|neovim|just|yazi)$"
            # export LD_PRELOAD="${pkgs.stderred}/lib/libstderred.so''${LD_PRELOAD:+:$LD_PRELOAD}"

            wim () { set -eu; ''${EDITOR:-vim} "$(which ''${1:?No file selected...})" ''${@:2}; set +eu }
            ? () {
              ${outputs.lib.getExe pkgs.krabby} random --no-title
            }
            magic-enter-cmd () {
              printf ' ?\n'
            }

            __git_files () {
              _wanted files expl 'local files' _files
            }

            ze () { "$EDITOR" "$("${outputs.lib.getExe config.programs.zoxide.package}" query "$@")" }
            zcode () { "${outputs.lib.getExe config.programs.vscode.package}" "$("${outputs.lib.getExe config.programs.zoxide.package}" query "$@")" }
            zd () { set -e; cd "$("${outputs.lib.getExe config.programs.zoxide.package}" query "$PWD" "$@")"; set +e }
            electron-wayland () { "''${1:?No program specificed...}" --enable-features=UseOzonePlatform,WaylandWindowDecorations --platform-hint=auto --ozone-platform=wayland "''${@:2}" }
            tmpvim () {
              revert () {
                mv "$1.bak" "$1"
              }

              if test ! -f "''${1:?No file specified...}"; then
                printf "File doesn't exist...\n"
                return 1
              fi

              trap "revert "$@"" EXIT

              mv "$1" "$1.bak"
              cat "$1.bak" > "$1"
              $EDITOR "$1"
            }

            bindkey '^G' per-directory-history-toggle-history
            bindkey -M vicmd '^G' per-directory-history-toggle-history

            autopair-init

            ${if config.modules.fastfetch.enable then outputs.lib.getExe pkgs.fastfetch else ""}
          '';
        in
        outputs.lib.mkMerge [
          zshConfigEarlyInit
          zshConfigBeforeCompInit
          zshConfig
        ];
      prezto = {
        enable = true;
        caseSensitive = false;
        terminal.autoTitle = true;
        editor.promptContext = true;
        ssh.identities = builtins.attrNames config.modules.ssh.keys;
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
