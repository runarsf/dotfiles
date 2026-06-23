_: {
  libExtensions = [
    {
      shell = {
        mkZoxideInit = pkgs: shellName:
          pkgs.runCommand "zoxide-init-${shellName}" {} "${pkgs.lib.getExe pkgs.zoxide} init ${shellName} > $out";
        mkStarshipInit = pkgs: shellName:
          pkgs.runCommand "starship-init-${shellName}" {} "${pkgs.lib.getExe pkgs.starship} init ${shellName} > $out";
        greeting = pkgs: "${pkgs.lib.getExe pkgs.krabby} random --no-title";

        aliases = pkgs: {
          tree = "${pkgs.lib.getExe pkgs.eza} --tree";
          cat = "${pkgs.lib.getExe pkgs.bat}";
          grep = "grep --color=always";
          docker-compose = "docker compose";
          dkcUf = "docker compose up -d --force-recreate";
        };

        abbrs = {
          nh = "niks";
        };
      };
    }
  ];
}
