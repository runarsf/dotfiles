{
  self,
  inputs,
  withSystem,
  ...
}: {
  flake.nixosConfigurations.modelland = withSystem "x86_64-linux" (
    {pkgs, ...}:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = with self.nixosModules; [
          modellandConfiguration
          homeManager
          {
            # home-manager.users.runar = {config, ...}: {
            # features.niks.flake = "${config.home.homeDirectory}/shared/dotfiles";
            # };
          }
        ];
      }
  );

  flake.nixosModules.modellandConfiguration = {
    config,
    pkgs,
    ...
  }: {
    imports = with self.nixosModules; [
      ./hardware-configuration.nix
      homeManager
      runar
    ];

    system.stateVersion = "26.05";
    networking.hostName = "modelland";

    services.power-profiles-daemon.enable = true;

    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;
    boot.kernelPackages = pkgs.linuxPackages_latest;
    time.timeZone = "Europe/Zurich";
    zramSwap.enable = true;
    swapDevices = [{device = "/swap/swapfile";}];
    services.xserver.videoDrivers = ["nvidia"];
    hardware.graphics = {
      enable = true;
      enable32Bit = true;
    };
    hardware.nvidia = {
      modesetting.enable = true;
      open = false;
      nvidiaSettings = true;
      powerManagement.enable = false;
      package = config.boot.kernelPackages.nvidiaPackages.legacy_580;
    };
    # services.desktopManager.plasma6.enable = true;
    # services.displayManager.plasma-login-manager.enable = true;
    # services.desktopManager.gnome.enable = true;
    # services.displayManager.gdm.enable = true;
    services.xserver.xkb.layout = "no";
    services.xserver.xkb.variant = "nb";
    # services.xserver.xkb.options = "eurosign:e,caps:escape";
    services.pipewire = {
      enable = true;
      pulse.enable = true;
    };
    networking.networkmanager.enable = true;

    environment.systemPackages = with pkgs; [
      git
      vim
    ];

    systemd.tmpfiles.rules = [
      "d /games 0755 runar users -"
    ];
  };
}
