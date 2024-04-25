{ pkgs, ... }:

{
  users = {
    groups.ops.gid = 420;
    users.ops = {
      uid = 420;
      isSystemUser = true;
      home = "/data";
      description = "Operations";
      group = "ops";
      createHome = true;
      linger = true;
      shell = pkgs.bash;
      extraGroups = [ "networkmanager" "podman" "docker" "libvirtd" ];
    };
  };
}
