{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "fun" {
  home = {
    packages = with pkgs; [gay blahaj krabby cowsay];

    shellAliases = {sl = "ls | rev";};
  };
}
