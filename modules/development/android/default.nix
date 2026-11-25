{
  lib',
  ...
}:
{
  flake.nixosModules.android = lib'.mkModule "android" (
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib) mkEnableOption optionals;

      cfg = config.modules.android;

      # androidenv.androidPkgs defaults to 5 platforms x 4 image types x 4 ABIs,
      # which is tens of GB of system images.
      sdk = pkgs.androidenv.composeAndroidPackages {
        platformVersions = [ "latest" ];
        systemImageTypes = [ "google_apis_playstore_ps16k" ];
        abiVersions = [ "x86_64" ];
        includeSystemImages = true;
        includeEmulator = true;
        includeNDK = true;
      };
    in
    {
      options.modules.android = {
        sdk = mkEnableOption "Android SDK from nixpkgs; Android Studio brings its own";
      };

      config = {
        users.groups = {
          adbusers.members = config.primaryUsers;
          plugdev.members = config.primaryUsers;
          kvm.members = config.primaryUsers;
        };

        environment.systemPackages =
          with pkgs;
          [
            android-tools
            jdk
          ]
          ++ optionals cfg.sdk [ sdk.androidsdk ];
      };
    }
  );

  flake.homeModules.android = lib'.mkModule "android" (
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib) optionals mkEnableOption;

      cfg = config.modules.android;
    in
    {
      options.modules.android = {
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
