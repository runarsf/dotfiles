_: {
  flake.nixosModules.nautilus = { pkgs, ... }: {
    services.gvfs.enable = true; # smb:// backend for Nautilus
    services.gnome.gnome-keyring.enable = true; # "remember password" (needed outside a full GNOME session)
    programs.dconf.enable = true; # Nautilus settings, if you're not on GNOME

    services.avahi = {
      enable = true;
      nssmdns4 = true;
    };

    environment.systemPackages = with pkgs; [ nautilus ];
  };
}
