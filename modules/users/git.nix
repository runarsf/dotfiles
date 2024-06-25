{ pkgs, outputs, ... }:

{
  # TODO Per directory git config https://stackoverflow.com/a/43884702
  #  https://git-scm.com/docs/git-config#_conditional_includes
  programs.git = {
    enable = true;

    diff-so-fancy.enable = true;

    userEmail = outputs.lib.mkDefault (throw "programs.git.userEmail is not set");
    userName = outputs.lib.mkDefault (throw "programs.git.userName is not set");

    extraConfig = ''
      [includeIf "gitdir:golog/"]
        path = ${pkgs.writeText ".gitconfig-golog" ''
          [user]
            name = Runar Fredagsvik
            email = runar.fredagsvik@golog.ch

          [core]
            sshCommand = ssh -i ~/.ssh/id_golog
      ''}
    '';

    aliases = rec {
      aliases = "config --get-regexp alias";

      quick = "!fn() { git add -A && git commit -m \"$*\" && git push; }; fn";
      again = "!fn() { git add -A && git commit --amend --no-edit --gpg-sign; }; fn";

      unstage = "reset --";
      discard = "!git reset --hard && git clean -df";

      recent = "log -3";
      latest = "log -1";
      last = latest;

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

  programs.gitui.enable = true;
}
