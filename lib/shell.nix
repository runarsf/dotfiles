_: {
  libExtensions = [
    {
      shell = {
        mkZoxideInit = pkgs: shellName:
          pkgs.runCommand "zoxide-init-${shellName}" {} "${pkgs.lib.getExe pkgs.zoxide} init ${shellName} > $out";
        mkStarshipInit = pkgs: shellName:
          pkgs.runCommand "starship-init-${shellName}" {} "${pkgs.lib.getExe pkgs.starship} init ${shellName} > $out";
        # home-manager's enable<Shell>Integration options emit the equivalent of
        # these as `eval "$(... init <shell>)"`, i.e. a process spawn on every
        # shell startup (3-10ms each). The output is static, so generate it once
        # at build time and source the result instead.
        mkCarapaceInit = pkgs: shellName:
          pkgs.runCommand "carapace-init-${shellName}" {} "${pkgs.lib.getExe pkgs.carapace} _carapace ${shellName} > $out";
        mkDirenvHook = pkgs: shellName:
          pkgs.runCommand "direnv-hook-${shellName}" {} "${pkgs.lib.getExe pkgs.direnv} hook ${shellName} > $out";
        greeting = pkgs: "${pkgs.lib.getExe pkgs.krabby} random --no-title";

        aliases = pkgs: {
          tree = "${pkgs.lib.getExe pkgs.eza} --tree";
          cat = "${pkgs.lib.getExe pkgs.bat}";
          grep = "grep --color=always";
          docker-compose = "docker compose";
          dkcUf = "docker compose up -d --force-recreate";
          ls = "EZA_ICON_SPACING=2 ${pkgs.lib.getExe pkgs.eza} -l -F -g -a --group-directories-first --no-time --git";
          develop = "nix develop --command $SHELL";
        };

        abbrs = {
          nh = "niks";
        };
      };
    }
  ];
}
