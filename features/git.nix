_: {
  flake.homeModules.git =
    {
      config,
      pkgs,
      lib,
      osConfig ? { },
      ...
    }:
    let
      inherit (lib)
        mkEnableOption
        mkOption
        getExe'
        mkDefault
        concatStringsSep
        optionalAttrs
        ;
      inherit (lib.types) listOf str nullOr;
      inherit (lib.strings) replaceString;

      cfg = config.features.git;
    in
    {
      options.features.git = {
        emailSelector = {
          enable = mkEnableOption "Enable email selector on clone";
          extraEmails = mkOption {
            type = listOf str;
            default = [ ];
            description = "Extra email addresses to use when committing.";
          };
        };

        signingKey = mkOption {
          default =
            let
              keys = osConfig.features.ssh.keys or [ ];
            in
            if keys == [ ] then
              null
            else
              builtins.trace "features.git.signingKey not set; defaulting to first SSH key '${(builtins.head keys).name}'. Set features.git.signingKey explicitly to suppress this warning." (builtins.head keys)
              .name;
          type = nullOr str;
          description = "Name of SSH key used for signing. Defaults to the first key in features.ssh.keys.";
        };
      };

      config = {
        programs = {
          git = {
            enable = true;

            lfs.enable = true;

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

              alias =
                let
                  mkGitFn =
                    body:
                    "!fn() { ${replaceString "\n" " " body} }; fn";
                in
                rec {
                  alias = "config --get-regexp alias";

                  quick = mkGitFn "git add -A && git commit --allow-empty -m \"$*\" && git push;";
                  again = mkGitFn "git add -A && git commit --amend --no-edit --gpg-sign;";

                  poop = mkGitFn ''git push && { ${getExe' pkgs.mplayer "mplayer"} "https://www.myinstants.com/media/sounds/fart-with-reverb.mp3" >/dev/null 2>&1; } || { ${getExe' pkgs.mplayer "mplayer"} "https://www.myinstants.com/media/sounds/fart-meme-sound_qo90QRs.mp3" >/dev/null 2>&1; };'';

                  unstage = "reset --";
                  discard = "!git reset --hard && git clean -df";

                  recent = "log -3";
                  latest = "log -1";
                  last = latest;

                  graph = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";

                  pull-all = mkGitFn "find . -type d -depth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull ';';";
                  discard-all = mkGitFn "git checkout main; git branch | grep -v 'main' | xargs git branch -D;";

                  purge = mkGitFn "git delete-all-branches; git fetch --prune; git reset --hard origin/main; git clean -df;";

                  hide = mkGitFn ''
                    if [ "$#" -eq 0 ]; then
                        echo 'Usage: git hide <file>...';
                        exit 1;
                    fi;
                    for file in "$@"; do git update-index --skip-worktree "$file"; done;
                  '';

                  unhide = mkGitFn ''
                    if [ "$#" -eq 0 ]; then
                        echo 'Usage: git unhide <file>...';
                        exit 1;
                    fi;
                    for file in "$@"; do git update-index --no-skip-worktree "$file"; done;
                  '';

                  stash-hidden = mkGitFn ''
                    if git stash list | grep -q 'skip-worktree-stash'; then
                      echo 'A skip-worktree-stash already exists — run "git unstash-hidden" first.';
                      exit 1;
                    fi;
                    files=$(git ls-files -v | grep '^S ' | cut -c3-);
                    if [ -z "$files" ]; then
                      echo 'No skip-worktree files found.';
                      exit 0;
                    fi;
                    printf '%s\n' "$files" | while IFS= read -r file; do
                      git update-index --no-skip-worktree "$file";
                    done;
                    git stash push -m skip-worktree-stash -- $files;
                    echo 'Stashed and unhid:'; printf '%s\n' "$files";
                  '';

                  unstash-hidden = mkGitFn ''
                    count=$(git stash list | grep -c 'skip-worktree-stash');
                    if [ "$count" -eq 0 ]; then
                        echo 'No skip-worktree-stash found.';
                        exit 1;
                    fi;
                    if [ "$count" -gt 1 ]; then
                        echo "Warning: $count skip-worktree-stash entries found; popping the most recent.";
                    fi;
                    ref=$(git stash list | grep -m1 'skip-worktree-stash' | cut -d: -f1);
                    files=$(git stash show --name-only "$ref");
                    git stash pop "$ref";
                    printf '%s\n' "$files" | while IFS= read -r file; do
                        git update-index --skip-worktree "$file";
                    done;
                    echo 'Popped and re-hid:'; printf '%s\n' "$files";
                  '';
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

        xdg.configFile."git/templates/hooks/post-checkout".source =
          let
            source = pkgs.writeShellApplication {
              name = "post-checkout";
              runtimeInputs = with pkgs; [ gum ];
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
                    ([ config.programs.git.settings.user.email ] ++ cfg.emailSelector.extraEmails)
                    |> map (email: ''"${email}"'')
                    |> concatStringsSep " "
                  })"

                git config --local user.email "$email"
              '';
            };
          in
          "${source}/bin/post-checkout";
      };
    };
}
