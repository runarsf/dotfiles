{config, ...}: {
  programs.ssh.matchBlocks = {
    skyhigh = {
      hostname = "10.212.173.1";
      user = "ubuntu";
      identityFile = "${config.home.homeDirectory}/.ssh/id_ntnu";
    };
  };
}
