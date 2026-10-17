{lib', ...}: {
  flake.nixosModules.nvidia = lib'.mkFeature "nvidia" (
    {
      config,
      lib,
      ...
    }: let
      inherit
        (lib)
        filterAttrs
        mkIf
        mkOption
        types
        ;

      cfg = config.features.nvidia;

      busIds = filterAttrs (_: v: v != null) {
        inherit (cfg.prime) amdgpuBusId intelBusId nvidiaBusId;
      };
    in {
      options.features.nvidia = {
        branch = mkOption {
          type = types.str;
          default = "production";
          example = "legacy_580";
          description = ''
            Attribute of `boot.kernelPackages.nvidiaPackages` to use as the
            driver package, e.g. "production", "beta", "legacy_580".
          '';
        };

        open = mkOption {
          type = types.bool;
          default = true;
          description = ''
            Use the open-source kernel modules. Requires Turing or newer; older
            cards (and the legacy driver branches) need this off.
          '';
        };

        powerManagement = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Preserve video memory across suspend/resume. Experimental, and the
            cause of more hangs than it fixes on desktops.
          '';
        };

        prime = {
          offload = mkOption {
            type = types.bool;
            default = busIds != {};
            defaultText = lib.literalMD "true if any of the prime bus IDs are set";
            description = ''
              Run the nvidia GPU in render-offload mode, keeping the integrated
              GPU as the primary display adapter. Also provides `nvidia-offload`.
            '';
          };

          amdgpuBusId = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "PCI:6:0:0";
            description = "Bus ID of the integrated AMD GPU, from `lspci`.";
          };

          intelBusId = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "PCI:0:2:0";
            description = "Bus ID of the integrated Intel GPU, from `lspci`.";
          };

          nvidiaBusId = mkOption {
            type = types.nullOr types.str;
            default = null;
            example = "PCI:1:0:0";
            description = "Bus ID of the discrete nvidia GPU, from `lspci`.";
          };
        };
      };

      config = {
        services.xserver.videoDrivers = ["nvidia"];

        hardware.graphics = {
          enable = true;
          enable32Bit = true;
        };

        hardware.nvidia = {
          inherit (cfg) open;

          modesetting.enable = true;
          nvidiaSettings = true;
          powerManagement.enable = cfg.powerManagement;
          package = config.boot.kernelPackages.nvidiaPackages.${cfg.branch};

          prime =
            busIds
            // {
              offload = mkIf cfg.prime.offload {
                enable = true;
                enableOffloadCmd = true;
              };
            };
        };
      };
    }
  );
}
