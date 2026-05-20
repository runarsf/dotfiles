{...}: {
  flake.nixosModules.blahaj = {pkgs, ...}: {
    users.users.blahaj = {
      isSystemUser = true;
      uid = 10001;
      home = "/var/lib/blahaj";
      createHome = true;
      shell = pkgs.bash;
      group = "blahaj";
    };
    users.groups.blahaj = {
      gid = 10001;
    };
  };
}
