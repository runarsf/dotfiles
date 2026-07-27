{self, ...}: {
  flake.nixosModules.osu = {pkgs, ...}: {
    # NOTE: Input mode must be set to "Artist Mode", see: https://opentabletdriver.net/Wiki/FAQ/LinuxAppSpecific
    hardware.opentabletdriver.enable = true;
  };

  flake.homeModules.osu = {pkgs, ...}: {
    home.packages = with pkgs.master; [
      osu-lazer-bin
    ];
  };
}
