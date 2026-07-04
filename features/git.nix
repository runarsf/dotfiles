_: {
  flake.homeModules.git = {
    config,
    pkgs,
    lib,
    osConfig ? {},
    ...
  }: let
    inherit (lib) mkEnableOption mkOption getExe' mkDefault concatStringsSep optionalAttrs;
    inherit (lib.types) listOf str nullOr;

    cfg = config.features.git;
  in {
    options.features.git = {
      emailSelector = {
        enable = mkEnableOption "Enable email selector on clone";
        extraEmails = mkOption {
          type = listOf str;
          default = [];
          description = "Extra email addresses to use when committing.";
        };
      };

      signingKey = mkOption {
        default = let
          keys = osConfig.features.ssh.keys or [];
        in
          if keys == []
          then null
          else
            builtins.trace
            "features.git.signingKey not set; defaulting to first SSH key '${(builtins.head keys).name}'. Set features.git.signingKey explicitly to suppress this warning."
            (builtins.head keys).name;
        type = nullOr str;
        description = "Name of SSH key used for signing. Defaults to the first key in features.ssh.keys.";
      };
    };

    config = {
      programs = {
        git = {
          enable = true;

          settings = {
            init.defaultBranch = "main";
            init.templateDir = "${config.xdg.configHome}/git/templates";
            pull.rebase = true;
            push.autoSetupRemote = true;
            rerere.enabled = true;

            user.email = mkDefault (throw "programs.git.settings.user.email is not set");
            user.name = mkDefault (throw "programs.git.settings.user.name is not set");

            signing = optionalAttrs (cfg.signingKey != null) {
              format = "ssh";
              signByDefault = true;
              key = "${config.home.homeDirectory}/.ssh/${cfg.signingKey}.pub";
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
                ''git push && { ${getExe' pkgs.mplayer "mplayer"} "https://www.myinstants.com/media/sounds/fart-with-reverb.mp3" >/dev/null 2>&1; } || { ${getExe' pkgs.mplayer "mplayer"} "https://www.myinstants.com/media/sounds/fart-meme-sound_qo90QRs.mp3" >/dev/null 2>&1; };'';

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
        };

        gitui.enable = true;
        gh.enable = true;
        delta = {
          enable = true;
          enableGitIntegration = true;
        };
      };

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
              --height "${toString (5 + (builtins.length cfg.emailSelector.extraEmails))}" \
              --no-show-help \
              ${
              ([config.programs.git.settings.user.email] ++ cfg.emailSelector.extraEmails)
              |> map (email: ''"${email}"'')
              |> concatStringsSep " "
            })"

            git config --local user.email "$email"
          '';
        };
      in "${source}/bin/post-checkout";
    };
  };
}
