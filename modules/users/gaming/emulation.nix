{
  config,
  pkgs,
  inputs,
  outputs,
  ...
}: let
  mkROM = {
    name,
    url,
    sha256,
    platform,
    path ? name,
  }: let
    pkg = pkgs.fetchzip {inherit url sha256;};
  in
    with outputs.lib; {
      enable = mkOption {
        type = types.bool;
        default = true;
      };
      package = mkOption {
        description = "Package for ${name}";
        type = types.attrsOf (
          types.attrsOf (
            types.either types.str types.bool
          )
        );
        default = {
          "${name}" = {
            enable = config.modules.emulation.roms."${name}".enable;
            source = "${pkg}/${path}";
            target = let
              ROM =
                path
                |> splitString "/"
                |> last;
            in "Emulation/ROMs/${platform}/${ROM}";
          };
        };
      };
    };
in
  outputs.lib.mkDesktopModule config "emulation" {
    options' = {
      roms = {
        nintendogs = mkROM {
          name = "nintendogs";
          platform = "DS";
          path = "Nintendogs - Labrador & Friends (Europe) (En,Fr,De,Es,It).nds";
          url = "https://myrient.erista.me/files/No-Intro/Nintendo%20-%20Nintendo%20DS%20(Decrypted)/Nintendogs%20-%20Labrador%20%26%20Friends%20(Europe)%20(En%2CFr%2CDe%2CEs%2CIt).zip";
          sha256 = "sha256-cSn7fg0YOK4J2niIbgiLy+t51s5bYaahjPGV3GDB444=";
        };
      };
    };

    config = {
      home.packages = [
        (inputs.melonds.packages.${pkgs.stdenv.hostPlatform.system}.default)
      ];

      home.file = outputs.lib.mergeAttrsList (with config.modules.emulation.roms; [
        (nintendogs.package)
      ]);

      sops.secrets = {
        ds-bios7 = {
          sopsFile = "${inputs.vault}/shared/DS/bios7.bin";
          format = "binary";
          path = "${config.home.homeDirectory}/Emulation/DS/system/bios7.bin";
        };
        ds-bios9 = {
          sopsFile = "${inputs.vault}/shared/DS/bios9.bin";
          format = "binary";
          path = "${config.home.homeDirectory}/Emulation/DS/system/bios9.bin";
        };
        ds-firmware = {
          sopsFile = "${inputs.vault}/shared/DS/firmware.bin";
          format = "binary";
          path = "${config.home.homeDirectory}/Emulation/DS/system/firmware.bin";
        };
      };
    };
  }
