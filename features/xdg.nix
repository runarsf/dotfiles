_: {
  flake.homeModules.xdg = _: {
    xdg = {
      enable = true;

      userDirs = {
        enable = true;
        createDirectories = true;
      };

      configFile."mimeapps.list".enable = false;
      dataFile."applications/mimeapps.list".force = true;
      mimeApps.enable = true;
    };
  };
}
