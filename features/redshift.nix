_: {
  flake.homeModules.redshift = _: {
    services.redshift = {
      enable = true;
      tray = true;
      provider = "geoclue2";
    };
  };

  flake.nixosModules.redshift = _: {
    nixos.services.geoclue2.enable = true;
  };
}
