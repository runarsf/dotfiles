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

  programs.git = {
    userName = "Thomas Espervik";
    userEmail = "thoesp@protonmail.com";
  };
}
