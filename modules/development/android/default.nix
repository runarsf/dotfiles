{
  lib',
  ...
}:
{
  flake.nixosModules.android =
    {
      config,
      pkgs,
      ...
    }:
    {
      users.groups = {
        adbusers.members = config.primaryUsers;
        plugdev.members = config.primaryUsers;
        kvm.members = config.primaryUsers;
      };
      environment.systemPackages = with pkgs; [
        android-tools
        androidenv.androidPkgs.androidsdk
        androidenv.androidPkgs.emulator
        androidenv.androidPkgs.ndk-bundle
        jdk
      ];
    };

  flake.homeModules.android = lib'.mkModule "android" (
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib)
        mkEnableOption
        mkOption
        optionals
        types
        ;

      cfg = config.modules.android;
    in
    {
      options.modules.android = with types; {
        enabled = mkOption {
          description = "If this module has been enabled (read-only, used to verify from other modules)";
          type = bool;
          default = true;
          readOnly = true;
        };
        ide = mkEnableOption "Android IDE";
      };

      config = {
        home.packages =
          with pkgs;
          [
            fvm
            scrcpy
            qtscrcpy

            # graphite2
            # gtk3
          ]
          ++ optionals cfg.ide [
            android-studio
          ];
      };
    }
  );
}
