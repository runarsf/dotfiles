{
  outputs,
  pkgs,
  config,
  ...
}:
outputs.lib.mkModule config ["dev" "java"] {
  options' = {
    ide = outputs.lib.mkEnableOption "Enable Java IDE";
  };

  config = {
    programs.java = {
      enable = true;
      package = pkgs.jdk21;
    };

    home.packages = with pkgs;
      outputs.lib.optionals (config.isDesktop && config.modules.dev.java.ide)
      [jetbrains.idea-ultimate];
  };
}
