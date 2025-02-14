{
  config,
  outputs,
  ...
}:
# TODO apc-extension on vscode https://github.com/drcika/apc-extension/issues/177
outputs.lib.mkDesktopModule config "vscode" {
  programs.vscode = {
    enable = true;
  };
}
