{
  config,
  outputs,
  ...
}:
outputs.lib.mkModule' config "xdg" true {
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };
}
