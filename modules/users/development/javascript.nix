{ outputs, pkgs, config, ... }:

outputs.lib.mkModule config "JavaScript" {
  home.packages = with pkgs.unstable; [
    nodejs
    nodePackages."@nestjs/cli"
  ];
}
