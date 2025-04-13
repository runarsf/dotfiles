{
  outputs,
  pkgs,
  config,
  ...
}:
# TODO set node prefix to .local/share/npm and npm/bin to path
outputs.lib.mkModule config "dev.javascript" {
  home.packages = with pkgs; [
    nodejs
  ];
}
