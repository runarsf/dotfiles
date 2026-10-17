_: {
  flake.nixosModules.zram =
    {
      config,
      lib,
      ...
    }:
    let
      inherit (lib) mkIf mkOption types;

      cfg = config.features.zram;
    in
    {
      options.features.zram = {
        swapfile = mkOption {
          type = types.nullOr types.str;
          default = null;
          example = "/swap/swapfile";
          description = "Disk swapfile to register alongside zram. Not created here.";
        };
      };

      config = {
        zramSwap.enable = true;

        swapDevices = mkIf (cfg.swapfile != null) [ { device = cfg.swapfile; } ];
      };
    };
}
