{
  outputs,
  config,
  keys ? [ ],
  ...
}:

{
  imports = [
    ./ssh.nix
    ./gpg.nix
  ];
}
// outputs.lib.mkModule config "keychain" {
  programs.keychain = {
    enable = true;
    agents = [
      "ssh"
      "gpg"
    ];
    inherit keys;
  };
}
