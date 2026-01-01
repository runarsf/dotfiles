_: {
  modules.git = {
    enable = true;
    emailSelector = {
      enable = true;
    };
  };

  programs.git.settings = {
    user.name = "Majus-Dev";
    user.email = "83781075+Majus-Dev@users.noreply.github.com";
  };
}
