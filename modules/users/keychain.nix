{ outputs, config, keys ? [ ], ... }:

{
  imports = [
    ./ssh.nix
    ./gpg.nix
  ];
} // outputs.lib.mkModule config "Keychain" {
  programs.keychain = {
    enable = true;
    agents = [ "ssh" "gpg" ];
    inherit keys;
  };
}
