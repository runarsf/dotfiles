_: {
  hosts.vm = {
    stateVersion = "25.11";
    users = [ "runar" ];
    modules = [ "sshServer" ];

    extraModules = [
      ./_hardware.nix
    ];

    configuration = { pkgs, ... }: {
      host.efi = false;

      boot.loader.grub = {
        enable = true;
        device = "/dev/vda";
        useOSProber = true;
      };

      services.xserver.enable = true;
      services.displayManager.sddm.enable = true;
      services.desktopManager.plasma6.enable = true;

      # Home-only modules otherwise, so root has no copy.
      environment.systemPackages = with pkgs; [
        git
        vim
      ];

      home-manager.users.runar = { config, ... }: {
        modules.niks.flake = "${config.home.homeDirectory}/shared/dotfiles";
      };
    };
  };
}
