{ lib, inputs, ... }: let
  inherit (lib) hasSuffix;
  inherit (inputs.nixlib.lib) deepMerge;
in {
  libExtensions = [
    {
      isLinux = hasSuffix "linux";
      isDarwin = hasSuffix "darwin";

      mkFor = system: hostname: {
        common ? {},
        systems ? {},
        hosts ? {},
      }: let
        systemConfig =
          if hasSuffix "linux" system && systems ? "linux"
          then systems.linux
          else if hasSuffix "darwin" system && systems ? "darwin"
          then systems.darwin
          else {};
        hostConfig =
          if hostname != null && hosts ? ${hostname}
          then hosts.${hostname}
          else {};
      in
        deepMerge [common systemConfig hostConfig];
    }
  ];
}
