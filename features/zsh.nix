{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.zsh = _: {
    programs.zsh.enable = true;
    programs.command-not-found.enable = false;
    environment.pathsToLink = ["/share/zsh"];
  };

  flake.homeModules.zsh = {
    pkgs,
    lib,
    ...
  }: {
    programs = {
      fzf.enable = true;
      zoxide.enable = true;
      command-not-found.enable = false;

      zsh = {
        enable = true;
        autosuggestion.enable = true;
        syntaxHighlighting.enable = true;
        enableCompletion = true;

        zsh-abbr = {
          enable = true;
          abbreviations = {
            nh = "niks";
          };
        };

        envExtra = ''
          AUTOPAIR_INHIBIT_INIT=1
          AUTOPAIR_SPC_WIDGET="abbr-expand-and-insert"
        '';

        plugins = let
          omz = name: {
            inherit name;
            src = pkgs.oh-my-zsh;
            file = "share/oh-my-zsh/plugins/${name}/${name}.plugin.zsh";
          };
        in
          with pkgs; [
            {
              name = "zsh-history-substring-search";
              src = zsh-history-substring-search;
              file = "share/zsh-history-substring-search/zsh-history-substring-search.zsh";
            }
            {
              name = "zsh-autopair";
              src = zsh-autopair;
              file = "share/zsh/zsh-autopair/autopair.zsh";
            }
            {
              name = "zsh-nix-shell";
              src = zsh-nix-shell;
              file = "share/zsh-nix-shell/nix-shell.plugin.zsh";
            }
            {
              name = "nix-zsh-completions";
              src = nix-zsh-completions;
              file = "share/zsh/plugins/nix/init.zsh";
            }
            {
              name = "zsh-docker-aliases";
              src = pkgs.fetchFromGitHub {
                owner = "akarzim";
                repo = "zsh-docker-aliases";
                rev = "6c3479abeef33362a7ec7e713a5a95f292fee9b9";
                sha256 = "0iycb90l83l09skcd2yy0kc112225psgd063cf950v7gjga7h46z";
              };
              file = "zsh-docker-aliases.plugin.zsh";
            }
            (omz "gitfast")
            (omz "dotenv")
            (omz "fancy-ctrl-z")
            (omz "per-directory-history")
          ];

        initContent = lib.mkMerge [
          (lib.mkOrder 550 ''
            autoload -Uz vcs_info
            vcs_info 'prompt'
          '')
          (lib.mkOrder 1000 ''
            setopt histignorealldups \
                   sharehistory \
                   menucomplete \
                   autoparamslash \
                   nonomatch
            unsetopt correct \
                     prompt_cr \
                     prompt_sp

            zstyle ':completion:*' special-dirs ..
            zstyle ':completion:*' special-dirs last
            zstyle ':completion:*' squeeze-slashes true
            zstyle ':completion:*' complete-options true
            _comp_options+=(globdots)

            typeset -A ZSH_HIGHLIGHT_REGEXP
            ZSH_HIGHLIGHT_REGEXP+=('[0-9]' fg=cyan)
            ZSH_HIGHLIGHT_HIGHLIGHTERS+=(main regexp)

            magic-enter-cmd () { ${lib.getExe pkgs.krabby} random --no-title }
            magic-enter () {
              if [[ -z $BUFFER ]]; then
                magic-enter-cmd
                zle accept-line
              else
                zle accept-line
              fi
            }
            zle -N magic-enter
            bindkey "^M" magic-enter

            ? () { ${lib.getExe pkgs.krabby} random --no-title }
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

            bindkey '^[[A' history-substring-search-up
            bindkey '^[[B' history-substring-search-down

            bindkey '^G' per-directory-history-toggle-history
            bindkey -M vicmd '^G' per-directory-history-toggle-history

            autopair-init
          '')
        ];

        prezto = {
          enable = true;
          caseSensitive = false;
          terminal.autoTitle = true;
          editor.promptContext = true;
          utility.safeOps = false;
          pmodules = [
            # "ssh"
            # "environment"
            # "terminal"
            # "history"
            # "directory"
            # "spectrum"
            # "utility"
            # "syntax-highlighting"
            # "autosuggestions"
            # "completion"
          ];
        };
      };
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
