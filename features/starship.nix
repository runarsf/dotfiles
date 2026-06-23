_: {
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
