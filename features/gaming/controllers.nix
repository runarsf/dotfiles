{
  lib',
  self,
  ...
}: {
  flake.nixosModules.controllers = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (builtins) concatStringsSep filter isPath isString readFile;
    inherit (lib) flatten mkOption toList;
    inherit (lib.types) enum either listOf;
    inherit (lib'.matching) matchStringList;
    inherit (pkgs.stdenv.hostPlatform) system;

    cfg = config.features.controllers;
  in {
    options.features.controllers = {
      rules = mkOption {
        type = let
          rules = enum ["generic" "steam" "dualsense" "8bitdo" "nintendo"];
        in
          either rules (listOf rules);
        default = ["generic" "steam"];
        apply = toList;
      };
    };

    config = {
      users.groups = {
        input.members = config.primaryUsers;
        uinput.members = config.primaryUsers;
        plugdev.members = config.primaryUsers;
      };

      hardware.uinput.enable = true;
      hardware.xpadneo.enable = true;

      boot.kernelModules = ["hid_nintendo" "hid-nintendo"];

      services.joycond.enable = true;

      services.udev = {
        packages = with pkgs;
          matchStringList cfg.rules [
            ["generic" game-devices-udev-rules]
            ["steam" steam]
          ];

        extraRules = let
          rules = matchStringList cfg.rules [
            ["8bitdo" ./rules/8bitdo.rules]
            ["dualsense" ./rules/dualsense.rules]
            ["nintendo" ./rules/nintendo.rules]
          ];
          paths = rules |> filter isPath |> map readFile;
          strings = rules |> filter isString;
        in
          [paths strings] |> flatten |> concatStringsSep "\n";
      };

      environment.sessionVariables = {
        SDL_GAMECONTROLLERCONFIG =
          self.packages.${system}.game-controller-db
          + "/gamecontrollerdb.txt";
      };
    };
  };
}
