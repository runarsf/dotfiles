{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule' config "dev.go" {
  ide = outputs.lib.mkEnableOption "Enable Go IDE";
} {
  home.packages = with pkgs;
    [go]
    ++ outputs.lib.optionals (config.isDesktop && config.modules.dev.go.ide)
    [jetbrains.goland];
}
