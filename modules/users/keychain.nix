{ keys ? [ ], ... }:

{
  programs.keychain = {
    enable = true;
    agents = [ "ssh" "gpg" ];
    keys = keys;
  };
}
