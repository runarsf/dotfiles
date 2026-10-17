_: {
  flake.nixosModules.host =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib)
        mkDefault
        mkIf
        mkMerge
        mkOption
        types
        ;
    in
    {
      options.host = {
        desktop = mkOption {
          type = types.bool;
          default = true;
          description = "Whether this host has a GUI session. GUI-only features gate on this.";
        };

        efi = mkOption {
          type = types.bool;
          default = true;
          description = "Boot with systemd-boot. Disable on hosts that use grub.";
        };
      };

      config = mkMerge [
        (mkIf config.host.efi {
          boot.loader.systemd-boot.enable = true;
          boot.loader.efi.canTouchEfiVariables = true;
        })

        {
          boot.kernelPackages = mkDefault pkgs.linuxPackages_latest;

          # Don't gate this on services.tlp.enable: nixos-hardware's laptop
          # profile already defaults tlp to off when this is on, and reading it
          # here closes the loop.
          services.power-profiles-daemon.enable = mkDefault config.host.desktop;
        }
      ];
    };
}
