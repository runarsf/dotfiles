{
  config,
  pkgs,
  outputs,
  ...
}:
# NOTE<For Wayland support in Jetbrains products, add '-Dawt.toolkit.name=WLToolkit' to [Help > Edit Custom VM Options...]"
outputs.lib.mkModule' config "dev.rust" {
  ide = outputs.lib.mkEnableOption "Enable Rust IDE";
} {
  home.packages = with pkgs;
    [cargo rustc]
    ++ outputs.lib.optionals (config.isDesktop && config.modules.dev.rust.ide)
    [jetbrains.rust-rover];

  PATH = ["${config.home.homeDirectory}/.cargo/bin"];
}
