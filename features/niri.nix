{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.niri = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    programs.niri = {
      enable = true;
      package = self.packages.${system}.niri;
    };
  };

  perSystem = {
    pkgs,
    lib,
    self',
    ...
  }: {
    packages.niri = inputs.wrapper-modules.wrappers.niri.wrap {
      inherit pkgs;

      settings = {
        input.keyboard = {
          xkb.layout = "nb,no";
        };

        layout.gaps = 5;

        xwayland-satellite.path = lib.getExe pkgs.xwayland-satellite;

        binds = {
          "Mod+Return".spawn-sh = lib.getExe pkgs.kitty;
          "Mod+Q".close-window = _: {};
          "Mod+D".spawn-sh = "${lib.getExe self'.packages.noctalia} ipc call launcher toggle";
        };

        spawn-at-startup = [
          (lib.getExe self'.packages.noctalia)
        ];
      };
    };
  };
}
