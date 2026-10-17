_: {
  flake.nixosModules.zram = {
    config,
    lib,
    ...
  }: let
    inherit (lib) mkIf mkOption types;

    cfg = config.features.zram;
  in {
    options.features.zram = {
      swapfile = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "/swap/swapfile";
        description = ''
          Disk swapfile to register in addition to zram, as a lower-priority
          tier for once zram fills up. The file itself is not created here;
          null registers no disk swap at all.
        '';
      };
    };

    config = {
      zramSwap.enable = true;

      swapDevices = mkIf (cfg.swapfile != null) [{device = cfg.swapfile;}];
    };
  };
}
