{ config, outputs, pkgs, ... }:

outputs.lib.mkModule' config "dev.C" {
  dev.c.ide = outputs.lib.mkEnableOption "Enable CLion";
  dev.cs.ide = outputs.lib.mkEnableOption "Enable Rider";
} {
  home.packages = with pkgs; [
    (with dotnetCorePackages; combinePackages [ sdk_6_0 sdk_7_0 sdk_8_0 ])
    cmake
    gcc
  ] ++ outputs.lib.optional config.dev.c.ide pkgs.unstable.jetbrains.clion
    ++ outputs.lib.optional config.dev.cs.ide pkgs.unstable.rider;
}