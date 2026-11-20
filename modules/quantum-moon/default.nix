_: {
  flake.homeModules.quantum-moon =
    {
      lib,
      pkgs,
      osConfig,
      ...
    }:
    let
      # `quantum-moon` runs the shell; `quantum-moon ipc call moon move` controls it.
      # The QML finds wallpaper.png and moon.png next to itself in ./shell.
      quantum-moon = pkgs.writeShellApplication {
        name = "quantum-moon";
        runtimeInputs = [ pkgs.quickshell ];
        text = ''exec qs -p ${./shell} "$@"'';
      };
    in
    {
      config = lib.mkIf (osConfig.host.desktop or true) {
        home.packages = [ quantum-moon ];

        # Both would draw on the background layer.
        services.hyprpaper.enable = lib.mkForce false;

        systemd.user.services.quantum-moon = {
          Unit = {
            Description = "Outer Wilds quantum moon wallpaper";
            PartOf = [ "graphical-session.target" ];
            After = [ "graphical-session.target" ];
          };
          Service = {
            ExecStart = lib.getExe quantum-moon;
            Restart = "on-failure";
          };
          Install.WantedBy = [ "graphical-session.target" ];
        };
      };
    };
}
