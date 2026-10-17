{
  self,
  withSystem,
  lib',
  ...
}: {
  flake.nixosConfigurations.bramble = lib'.mkHost {
    inherit self withSystem;
    configuration = {
      pkgs,
      lib,
      ...
    }: let
      inherit (lib) mkForce;

      hostFeatures = lib'.useFeatures self [
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
    in {
      imports = with self.nixosModules;
        [
          ./hardware-configuration.nix
          host
          homeManager
          runar
        ]
        ++ hostFeatures.nixos;

      features.fonts.oled = true;
      home-manager.users.runar = {
        imports = hostFeatures.home;

        home.packages = [pkgs.bruno];

        wayland.windowManager.hyprland.settings.config.input.numlock_by_default = mkForce false;
      };

      system.stateVersion = "26.05";
      networking.hostName = "bramble";

      time.timeZone = mkForce "Europe/Zurich";

      services.libinput.enable = true;
      programs.localsend.enable = true;

      environment.systemPackages = with pkgs; [
        teams-for-linux
        # self.packages.${pkgs.stdenv.hostPlatform.system}.zaku
      ];
    };
  };
}
