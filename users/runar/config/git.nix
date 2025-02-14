{
  config,
  pkgs,
  ...
}: {
  # TODO Per directory git config https://stackoverflow.com/a/43884702
  #  https://git-scm.com/docs/git-config#_conditional_includes
  programs.git = {
    userName = "Runar Fredagsvik";
    userEmail = "i@runar.ch";
  };
}
