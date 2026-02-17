{
  pkgs,
  outputs,
  config,
  ...
}: let
  cfg = config.modules.git;
in
  outputs.lib.mkModule config "git" {
    options' = with outputs.lib; {
      emailSelector = {
        enable = mkEnableOption "Enable email selector on clone";
        extraEmails = mkOption {
          type = types.listOf types.str;
          default = [];
          description = "Extra email addresses to use when committing.";
        };
      };
    };

    config = {
      programs.delta = {
        enable = true;
        enableGitIntegration = true;
      };

      programs.git = {
        enable = true;

        settings = {
          init.defaultBranch = "main";
          init.templateDir = "${config.xdg.configHome}/git/templates";
          pull.rebase = true;
          push.autoSetupRemote = true;

          user.email = outputs.lib.mkDefault (throw "programs.git.settings.user.email is not set");
          user.name = outputs.lib.mkDefault (throw "programs.git.settings.user.name is not set");

          signing = outputs.lib.optionalAttrs (config.modules.ssh.signingKey != null) {
            format = "ssh";
            signByDefault = true;
            key = "${config.home.homeDirectory}/.ssh/${config.modules.ssh.signingKey}.pub";
          };

          alias = let
            mkGitFn = body:
            # bash
            "!fn() { ${body} }; fn";
          in rec {
            alias = "config --get-regexp alias";

            quick = mkGitFn "git add -A && git commit --allow-empty -m \"$*\" && git push;";
            again = mkGitFn "git add -A && git commit --amend --no-edit --gpg-sign;";

            poop =
              mkGitFn
              ''git push && { ${outputs.lib.getExe' pkgs.mplayer "mplayer"} "https://www.myinstants.com/media/sounds/fart-with-reverb.mp3" >/dev/null 2>&1; } || { ${outputs.lib.getExe' pkgs.mplayer "mplayer"} "https://www.myinstants.com/media/sounds/fart-meme-sound_qo90QRs.mp3" >/dev/null 2>&1; };'';

            unstage = "reset --";
            discard = "!git reset --hard && git clean -df";

            recent = "log -3";
            latest = "log -1";
            last = latest;

            graph = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";

            pull-all = mkGitFn "find . -type d -depth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull ';';";
            discard-all = mkGitFn "git checkout main; git branch | grep -v 'main' | xargs git branch -D;";

            purge = mkGitFn "git delete-all-branches; git fetch --prune; git reset --hard origin/main; git clean -df;";
          };
        };

        # TODO Make this an option: modules.git.pathConfig
        # extraConfig = {
        #   "includeIf \"gitdir:~/Development/edu/\"" = {
        #     path = builtins.toString (
        #       pkgs.writeText ".gitconfig-ntnu" ''
        #         [core]
        #           sshCommand = ssh -i ~/.ssh/id_ntnu

        #         [user]
        #           name = Runar Fredagsvik
        #           email = runarsfr@stud.ntnu.no
        #       ''
        #     );
        #   };
        # };
      };

      programs.gitui.enable = true;

      xdg.configFile."git/templates/hooks/post-checkout".source = let
        source = pkgs.writeShellApplication {
          name = "post-checkout";
          runtimeInputs = with pkgs; [gum];
          text = ''
            if [ "$1" != "0000000000000000000000000000000000000000" ]; then
              exit 0
            fi

            remote_url="$(git config --get remote.origin.url)"
            case "$remote_url" in
              *github.com*) exit 0;;
            esac

            email="$(gum filter \
              --no-strict \
              --limit 1 \
              --header "Select email for this repository" \
              --placeholder "Email..." \
              --prompt "> " \
              --indicator "»" \
              --height "${builtins.toString (5 + (builtins.length cfg.emailSelector.extraEmails))}" \
              --no-show-help \
              ${
              ([config.programs.git.settings.user.email] ++ cfg.emailSelector.extraEmails)
              |> builtins.map (email: ''"${email}"'')
              |> outputs.lib.concatStringsSep " "
            })"

            git config --local user.email "$email"
          '';
        };
      in "${source}/bin/post-checkout";
    };
  }
