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

        (
          pkgs.writers.writePython3Bin "find-cmake-target" {doCheck = false;}
          (builtins.readFile ./find-cmake-target.py)
        )
      ]
      ++ outputs.lib.optionals (config.isDesktop && config.modules.dev.c.ide)
      [jetbrains.clion];
  };
}
