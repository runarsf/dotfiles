_:

{
  services.ssh-agent.enable = true;
  programs.ssh.addKeysToAgent = "yes";
  system.programs.ssh.startAgent = true;
}
