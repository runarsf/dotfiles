{ pkgs, config, ... }:

{
  home.packages = with pkgs; [ sops ];
  sops.age.sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/aegissh" ];
}
