{ pkgs, outputs, ... }:

{
  environment.systemPackages = with pkgs; [ greetd.tuigreet ];

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = outputs.lib.mkDefault "${pkgs.greetd.tuigreet}/bin/tuigreet --time --remember";
      };
    };
  };

  systemd.services.greetd.serviceConfig = {
    Type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal";
    TTYReset = true;
    TTYVHangup = true;
    TTYVDisallocate = true;
  };

  environment.etc."greetd/environments".text = ''
    Hyprland
    zsh
    dbus-run-session -- gnome-shell --display-server --wayland
    startx
  '';
}
