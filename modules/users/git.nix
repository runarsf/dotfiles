{ outputs, ... }:

{
  programs.git = {
    enable = true;

    diff-so-fancy.enable = true;

    userEmail = outputs.lib.mkDefault (throw "programs.git.userEmail is not set");
    userName = outputs.lib.mkDefault (throw "programs.git.userName is not set");

    aliases = rec {
      aliases = "config --get-regexp alias";
      alii = aliases;

      gud = "!echo hello";

      lazy = ''!fn() { git add -A && git commit -m "$*" && git push; }; fn'';
      again = "commit --amend --no-edit --gpg-sign";

      unstage = "reset --";
      discard = "!git reset --hard && git clean -df";

      latest = "log -1";
      recent = "log -3";

      graph =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";

      pull-all =
        "!fn() { find . -type d -depth 1 -exec git --git-dir={}/.git --work-tree=$PWD/{} pull ';'; }; fn";
      discard-all =
        "!fn() { git checkout main; git branch | grep -v 'main' | xargs git branch -D; }; fn";

      purge =
        "!fn() { git delete-all-branches; git fetch --prune; git reset --hard origin/main; git clean -df; }; fn";
    };
  };
}
