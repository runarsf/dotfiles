{
  self,
  lib',
  ...
}: {
  flake.homeModules.emulation = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkOption types;
    inherit (lib'.matching) matchStringList;

    cfg = config.features.emulation;
  in {
    options.features.emulation = with types; {
      consoles = mkOption {
        type = listOf <| enum ["ds" "wii" "psp" "retroarch"];
        default = [];
      };
    };

    config = {
      home.packages = with pkgs;
        matchStringList cfg.consoles [
          ["ds" melonds]
          ["wii" dolphin-emu]
          ["psp" ppsspp]
          ["retroarch" retroarch-full]
        ];

      sops.secrets = {
        ds-bios7 = {
          sopsFile = "${self.lib.vaultPath}/shared/DS/bios7.bin";
          format = "binary";
          path = "${config.home.homeDirectory}/Emulation/DS/system/bios7.bin";
        };
        ds-bios9 = {
          sopsFile = "${self.lib.vaultPath}/shared/DS/bios9.bin";
          format = "binary";
          path = "${config.home.homeDirectory}/Emulation/DS/system/bios9.bin";
        };
        ds-firmware = {
          sopsFile = "${self.lib.vaultPath}/shared/DS/firmware.bin";
          format = "binary";
          path = "${config.home.homeDirectory}/Emulation/DS/system/firmware.bin";
        };
      };
    };
  };
}
