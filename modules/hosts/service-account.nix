{ pkgs, ... }:

{
  users.users.ops = {
    isSystemUser = true;
    home = "/data";
    description = "Operations";
    initialPassword = "!";
    shell = pkgs.bash;
    extraGroups = [ "networkmanager" "docker" "libvirtd" ];
  };
  programs.bash.enable = true;
}
