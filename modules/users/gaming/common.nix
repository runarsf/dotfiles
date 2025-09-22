{
  outputs,
  config,
  pkgs,
  name,
  ...
}:
outputs.lib.mkDesktopModule config "gaming" {
  nixos = {
    users.users."${name}".extraGroups = ["input" "uinput" "plugdev"];

    hardware.uinput.enable = true;
    hardware.xpadneo.enable = true;

    boot.kernelModules = ["hid_nintendo" "hid-nintendo"];

    services.joycond.enable = true;

    services.udev = {
      packages = with pkgs; [
        game-devices-udev-rules
        steam
      ];
    };

    environment.sessionVariables = {
      SDL_GAMECONTROLLERCONFIG =
        pkgs.fetchFromGitHub {
          owner = "mdqinc";
          repo = "SDL_GameControllerDB";
          rev = "992a0caf690e32a332a9707c355a4444516a2764";
          sha256 = "sha256-hv1xtAXpSQlzO1nSUkFaeoth4o0V7aUjzZgqnehezaY=";
        }
        + "/gamecontrollerdb.txt";
    };
  };

  modules.udev.extraRules = [ ./dualsense.rules ./8bitdo.rules ];
}
