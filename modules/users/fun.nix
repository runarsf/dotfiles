{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "fun" rec {
  home = {
    packages = with pkgs; [gay blahaj krabby cowsay];

    shellAliases = {sl = "ls | rev";};
  };

  programs.nushell.shellAliases = {
    inherit (home.shellAliases) sl;
  };
}
