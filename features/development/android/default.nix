{self, ...}: {
  flake.nixosModules.android = {
    config,
    lib,
    pkgs,
    ...
  }: {
    users.groups = {
      adbusers.members = config.primaryUsers;
      plugdev.members = config.primaryUsers;
      kvm.members = config.primaryUsers;
    };
    environment.systemPackages = with pkgs; [android-tools];
  };

  flake.homeModules.android = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkEnableOption optionals;

    cfg = config.features.android;
  in {
    options.features.android = {
      ide = mkEnableOption "Android IDE";
    };

    config = {
      home.packages = with pkgs;
        [
          fvm
          scrcpy
          qtscrcpy

          # graphite2
          # gtk3
        ]
        ++ optionals cfg.ide [
          unstable.android-studio
        ];
    };
  };
}
