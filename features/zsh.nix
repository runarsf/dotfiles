{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.zsh = {pkgs, ...}: {
    programs.zsh.enable = true;
    programs.command-not-found.enable = false;
    environment.pathsToLink = ["/share/zsh"];
    environment.shells = [self.packages.${pkgs.stdenv.hostPlatform.system}.zsh];
  };

  flake.homeModules.zsh = {pkgs, ...}: {
    programs.zsh = {
      enable = true;
      package = self.packages.${pkgs.stdenv.hostPlatform.system}.zsh;
    };
  };

  perSystem = {
    pkgs,
    lib,
    self',
    ...
  }: let
    inherit (lib) getExe concatMapAttrsStringSep concatMapStringsSep;

    abbrs = {
      nh = "niks";
    };

    aliases = {
      ls = "EZA_ICON_SPACING=2 ${getExe pkgs.eza} -l -F -g -a --group-directories-first --no-time --git";
      tree = "${getExe pkgs.eza} --tree";
      cat = "${getExe pkgs.bat}";
      grep = "grep --color=always";
      develop = "nix develop --command zsh";
      docker-compose = "docker compose";
      dkcUf = "docker compose up -d --force-recreate";
    };

    eagerPlugins = [
      "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
      "${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh"
      "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/init.zsh"
      "${pkgs.zsh-abbr}/share/zsh/zsh-abbr/zsh-abbr.plugin.zsh"
      "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/gitfast/gitfast.plugin.zsh"
      "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/dotenv/dotenv.plugin.zsh"
      "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fancy-ctrl-z/fancy-ctrl-z.plugin.zsh"
      "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/per-directory-history/per-directory-history.plugin.zsh"
      "${pkgs.oh-my-zsh}/share/oh-my-zsh/lib/key-bindings.zsh"
      "${pkgs.fzf}/share/fzf/completion.zsh"
      "${pkgs.fzf}/share/fzf/key-bindings.zsh"
      "${self'.packages.zsh-docker-aliases}/zsh-docker-aliases.plugin.zsh"
    ];

    deferredPlugins = [
      "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
      "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
      "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
    ];
  in {
    packages.zsh = inputs.wrapper-modules.wrappers.zsh.wrap {
      inherit pkgs;

      zshenv.content = ''
        AUTOPAIR_INHIBIT_INIT=1
        AUTOPAIR_SPC_WIDGET="abbr-expand-and-insert"
      '';

      zshrc.content = ''
        # Workaround for non-nixos machines that can't set pathsToLink
        if (( ! ''${fpath[(I)/run/current-system/sw/share/zsh/site-functions]} )); then
          for p in ''${(z)NIX_PROFILES}; do
            fpath=($p/share/zsh/site-functions $p/share/zsh/''${ZSH_VERSION}/functions $fpath)
          done
        fi

        fpath=(''${XDG_DATA_HOME:-$HOME/.local/share}/zsh/generated_man_completions $fpath)

        # Initialize zsh completions system
        # https://zsh.sourceforge.io/Doc/Release/Zsh-Modules.html
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

        source ${pkgs.zsh-defer}/share/zsh-defer/zsh-defer.plugin.zsh

        if [[ ! -d ''${XDG_DATA_HOME:-$HOME/.local/share}/zsh/generated_man_completions ]]; then
          zsh-defer ${getExe self'.packages.zsh-manpage-completion-generator}
        fi

        ${eagerPlugins |> concatMapStringsSep "\n" (p: "source ${p}")}

        typeset -A ZSH_HIGHLIGHT_REGEXP
        ZSH_HIGHLIGHT_REGEXP+=('[0-9]' fg=cyan)
        ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main regexp)

        ${deferredPlugins |> concatMapStringsSep "\n" (p: "zsh-defer source ${p}")}
        zsh-defer bindkey '^[[A' history-substring-search-up
        zsh-defer bindkey '^[[B' history-substring-search-down

        ${abbrs |> concatMapAttrsStringSep "\n" (k: v: "abbr add --quiet '${k}=${v}'")}
        ${aliases |> concatMapAttrsStringSep "\n" (k: v: "alias ${k}='${v}'")}

        source ${pkgs.runCommand "starship-init-zsh" {} "${getExe pkgs.starship} init zsh > $out"}
        source ${pkgs.runCommand "zoxide-init-zsh" {} "${getExe pkgs.zoxide} init zsh > $out"}

        vcs_info 'prompt'

        # https://zsh.sourceforge.io/Doc/Release/Options.html
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

        zstyle ':completion:*' menu select
        zstyle ':completion:*:default' list-colors '''
        zstyle ':completion:*' special-dirs ..
        zstyle ':completion:*' special-dirs last
        zstyle ':completion:*' squeeze-slashes true
        zstyle ':completion:*' complete-options true
        # case-insensitive, partial-word, and then substring completion
        zstyle ':completion:*' matcher-list ''' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
        zstyle ':completion:*' completer _complete _match _approximate _prefix
        zstyle ':completion:*:approximate:*' max-errors 1 numeric
        zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'
        zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
        _comp_options+=(globdots)

        magic-enter-cmd () { ${getExe pkgs.krabby} random --no-title }
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

        autopair-init
      '';
    };
  };

  flake.homeModules.starship = _: {
    programs.starship = {
      enable = true;

      settings = {
        format = "(($username(@$hostname))( $directory)( $git_branch( $git_commit)( $git_state)( $git_status))( $shlvl)( $cmd_duration)( $package)( $direnv)(- $aws$gcloud$openstack)($jobs) )$character";
        add_newline = false;

        character = {
          error_symbol = "[×](bold red)";
          success_symbol = "[»](bold green)";
          vimcmd_symbol = "[«](bold yellow)";
          vimcmd_visual_symbol = "[«](bold cyan)";
          vimcmd_replace_symbol = "[«](bold purple)";
          vimcmd_replace_one_symbol = "[«](bold purple)";
        };

        username = {
          format = "[$user]($style)";
          show_always = true;
        };
        hostname = {
          format = "[$hostname]($style)";
          ssh_only = true;
          style = "bold green";
        };
        directory = {
          format = "[$path]($style)([:$read_only]($read_only_style))";
          truncation_length = 2;
          read_only = " ";
        };

        shlvl = {
          format = "$symbol[$shlvl]($style)";
          style = "yellow";
          symbol = "󰧾 ";
          threshold = 2;
          disabled = false;
        };
        cmd_duration = {
          format = "󱑍 [$duration]($style)";
          style = "yellow";
          min_time = 10000;
          show_milliseconds = false;
        };

        git_branch = {
          format = "[$branch(:$remote_branch)]($style)";
          style = "purple";
          symbol = "";
        };
        git_commit = {format = "[\\($hash$tag\\)]($style)";};
        git_state = {format = "\\([$state( $progress_current/$progress_total)]($style)\\)";};
        git_metrics = {format = "([+$added]($added_style))( [-$deleted]($deleted_style))";};
        git_status = {
          format = "([\\[$all_status$ahead_behind\\]]($style))";
          deleted = "×";
          style = "red";
        };

        nix_shell = {
          format = "[($name \\(dev\\) ← )$symbol]($style)";
          impure_msg = "";
          symbol = " ";
          style = "cyan";
          heuristic = true;
        };
        package = {
          format = "[$symbol$version]($style)";
          symbol = "󰏗 ";
        };
        gcloud = {
          format = "[$symbol$active(/$project)(\\($region\\))]($style)";
          symbol = " ";
        };
        aws = {
          format = "[$symbol$profile(\\($region\\))]($style)";
          symbol = "  ";
        };

        conda.symbol = " ";
        dart.symbol = " ";
        docker_context.symbol = " ";
        elixir.symbol = " ";
        elm.symbol = " ";
        golang.symbol = " ";
        java.symbol = " ";
        julia.symbol = " ";
        memory_usage.symbol = "󰍛 ";
        nim.symbol = "󰆥 ";
        nodejs.symbol = " ";
        perl.symbol = " ";
        php.symbol = " ";
        python.symbol = " ";
        ruby.symbol = " ";
        rust.symbol = " ";
        scala.symbol = " ";
        swift.symbol = "󰛥 ";
        terraform.symbol = "󱁢 ";
      };
    };
  };
}
