{
  outputs,
  config,
  ...
}:
outputs.lib.mkDesktopModule config "gaming" {
  modules.logitech.enable = true;
}
