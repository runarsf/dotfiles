{ config, outputs, ... }:

outputs.lib.mkEnabledModule config "xdg" {
  xdg.userDirs = {
    enable = true;
    createDirectories = true;
  };
}
