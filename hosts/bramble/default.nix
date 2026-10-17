{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.bramble = lib'.mkHost {
    inherit self withSystem;

    hostName = "bramble";
    stateVersion = "26.05";
    users = ["runar"];

    features = [
      "yubikey"
      "web"
      "sql"
      "easyeffects"
      {
        csharp = {
          ide = true;
        };
      }
      {
        nvidia = {
          branch = "production";
          open = true;
          prime = {
            amdgpuBusId = "PCI:6:0:0";
            nvidiaBusId = "PCI:1:0:0";
          };
        };
      }
      {
        zram = {
          swapfile = "/swap/swapfile";
        };
      }
      # {
      #   hyprland = {
      #     animations = true;
      #   };
      # }
    ];

    imports = [./hardware-configuration.nix];

    configuration = {
      pkgs,
      lib,
      ...
    }: {
      time.timeZone = lib.mkForce "Europe/Zurich";

      services.libinput.enable = true;
      programs.localsend.enable = true;

      environment.systemPackages = with pkgs; [
        teams-for-linux
        # self.packages.${pkgs.stdenv.hostPlatform.system}.zaku
      ];

      features.fonts.oled = true;

      home-manager.users.runar = {
        home.packages = [pkgs.bruno];

        wayland.windowManager.hyprland.settings.config.input.numlock_by_default = lib.mkForce false;
      };
    };
  };
}
