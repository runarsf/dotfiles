_: {
  flake.nixosModules.host = {
    config,
    lib,
    pkgs,
    ...
  }: let
    inherit (lib) mkDefault mkOption types;
  in {
    options.host.desktop = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Whether this host has a desktop/GUI session, as opposed to being a
        headless server. GUI-only features gate on this so they can stay in
        the shared feature list and simply no-op on headless hosts.
      '';
    };

    config = {
      boot.loader = {
        systemd-boot.enable = mkDefault (!config.boot.loader.grub.enable);
        efi.canTouchEfiVariables = mkDefault config.boot.loader.systemd-boot.enable;
      };

      boot.kernelPackages = mkDefault pkgs.linuxPackages_latest;

      services.power-profiles-daemon.enable = mkDefault (
        config.host.desktop && !config.services.tlp.enable && !config.services.auto-cpufreq.enable
      );
    };
  };
}
