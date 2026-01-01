_: {
  modules.git = {
    enable = true;
    emailSelector = {
      enable = true;
      extraEmails = [
        "thomes@stud.ntnu.no"
      ];
    };
  };

  programs.git.settings = {
    user.name = "Thomas Espervik";
    user.email = "thoesp@protonmail.com";
  };
}
