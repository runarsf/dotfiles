{
  pkgs,
  name,
  outputs,
  config,
  ...
}:
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
        graphite2
        gtk3
        android-tools
        scrcpy
        qtscrcpy
        fvm
        (
          pkgs.writeShellApplication {
            name = "adbrute";
            runtimeInputs = with pkgs; [
              android-tools
              inetutils
              gnused
              nmap
            ];
            text = builtins.readFile ./adbrute.sh;
          }
        )
      ]
      ++ outputs.lib.optionals (config.isDesktop && config.modules.dev.android.ide) [android-studio];

    nixpkgs.config.android_sdk.accept_license = true;
  };
}
