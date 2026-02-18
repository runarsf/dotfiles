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
        users.users."${name}".extraGroups = [
          "adbusers"
          "plugdev"
          "kvm"
        ];
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
          unstable.android-studio
        ];

      nixpkgs.config.android_sdk.accept_license = true;
    };
  }
