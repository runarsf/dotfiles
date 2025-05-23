{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config ["dev" "c"] {
  options' = {
    ide = outputs.lib.mkEnableOption "Enable C IDE";
  };

  config = {
    home.packages = with pkgs;
      [
        (with dotnetCorePackages; combinePackages [sdk_6_0 sdk_7_0 sdk_8_0])
        cmake
        gcc
      ]
      ++ outputs.lib.optionals (config.isDesktop && config.modules.dev.c.ide)
      [jetbrains.clion];
  };
}
