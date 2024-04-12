_:

{
  services.ssh-agent.enable = true;
  programs.ssh.addKeysToAgent = "yes";
  nixos.programs.ssh.startAgent = true;
}
