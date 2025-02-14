{
  inputs,
  pkgs,
  outputs,
  lib,
  ...
}: let
  # TODO Turn into list
  hyprland-session = "${inputs.hyprland.packages.${pkgs.system}.hyprland}/share/wayland-sessions";
  gnome-session = "${pkgs.gnome.gnome-session}/share/gnome-session";
in {
  environment.systemPackages = with pkgs; [greetd.tuigreet];

  services.greetd = {
    enable = true;
    settings = {
      vt = 1;
      default_session = {
        # echo gnome-session.outPath | nix repl nixpkgs#gnome
        command = outputs.lib.mkDefault (lib.concatStringsSep " " [
          "${pkgs.greetd.tuigreet}/bin/tuigreet"
          "--time"
          "--remember"
          "--remember-session"
          "--cmd Hyprland"
          # "--sessions '${hyprland-session}'"
          # "--xsessions '${gnome-session}'"
        ]);
        user = "greeter";
      };
    };
  };

  services.xserver = {
    enable = true;
    # displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;
  };
  services.gnome = {
    core-utilities.enable = true;
    core-shell.enable = true;
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

  # For gtkgreet
  # environment.etc."greetd/environments".text = ''
  #   Hyprland
  #   zsh
  #   dbus-run-session -- gnome-shell --display-server --wayland
  #   startx
  # '';
}
