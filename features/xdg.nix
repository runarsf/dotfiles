{ ... }: {
  flake.homeModules.xdg = { ... }: {
    xdg = {
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
