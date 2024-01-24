{ outputs, ... }:

with outputs.lib;

{
  isLinux = hasSuffix "linux";
  isDarwin = hasSuffix "darwin";

  isWayland = config: config.home.sessionVariables ? NIXOS_OZONE_WL;

  mkFor = system: hostname:
    { common ? { }, systems ? { }, hosts ? { } }:
    let
      systemConfig = if isLinux system && systems ? "linux" then
        systems.linux
      else if isDarwin system && systems ? "darwin" then
        systems.darwin
      else
        { };
      hostConfig = if hostname != null && hosts ? ${hostname} then
        hosts.${hostname}
      else
        { };
    in deepMerge [ common systemConfig hostConfig ];
}
