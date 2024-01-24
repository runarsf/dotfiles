{ keys ? [ ], ... }:

{
  services.ssh-agent.enable = true;
  programs.ssh.addKeysToAgent = "yes";

  programs.keychain = {
    enable = true;
    agents = [ "ssh" "gpg" ];
    keys = keys;
  };
}
