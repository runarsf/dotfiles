{ config, pkgs, ... }:

{
  # TODO Per directory git config https://stackoverflow.com/a/43884702
  #  https://git-scm.com/docs/git-config#_conditional_includes
  programs.git = {
    userName = "Runar Fredagsvik";
    userEmail = "i@runar.ch";

    extraConfig = {
      "includeIf \"gitdir:~/Development/edu/\"" = {
        path = builtins.toString (
          pkgs.writeText ".gitconfig-ntnu" ''
            [core]
              sshCommand = ssh -i ~/.ssh/id_work

            [user]
              name = Runar Fredagsvik
              email = runarsfr@stud.ntnu.no
          ''
        );
      };
    };
  };
}
