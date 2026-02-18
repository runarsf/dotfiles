_: {
  modules.git = {
    enable = true;
    emailSelector = {
      enable = true;
      extraEmails = [
        "runarsfr@stud.ntnu.no"
      ];
    };
  };

  programs.git.settings = {
    user.name = "Runar Fredagsvik";
    user.email = "i@runar.ch";
  };
}
