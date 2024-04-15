{ keys ? [ ], ... }:

{
  imports = [
    ./ssh.nix
    ./gpg.nix
  ];

  programs.keychain = {
    enable = true;
    agents = [ "ssh" "gpg" ];
    keys = keys;
    enableZshIntegration = false;
  };
}
