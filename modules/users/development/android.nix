{
  pkgs,
  name,
  outputs,
  config,
  ...
}: let
  self = config.modules.dev.android;
in
  outputs.lib.mkModule config ["dev" "android"]
  {
    options' = {
      ide = outputs.lib.mkEnableOption "Enable Android IDE";
    };

    config = {
      nixos = {
        programs.adb.enable = true;
        users.users."${name}".extraGroups = [
          "adbusers"
          "plugdev"
          "kvm"
        ];
        services.udev.packages = with pkgs; [android-udev-rules];
        environment.systemPackages = with pkgs; [android-tools];
      };

      home.packages = with pkgs;
        [
          fvm
          scrcpy
          qtscrcpy

          # graphite2
          # gtk3
        ]
        ++ outputs.lib.optionals (config.isDesktop && self.ide) [
          android-studio
          jetbrains.idea-ultimate
        ];

      nixpkgs.config.android_sdk.accept_license = true;
    };
  }
