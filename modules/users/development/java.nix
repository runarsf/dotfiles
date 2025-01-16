{ outputs, pkgs, config, ... }:

outputs.lib.mkModule' config "dev.java" {
  ide = outputs.lib.mkEnableOption "Enable Java IDE";
} {
  programs.java = {
    enable = true;
    package = pkgs.jdk21;
  };

  home.packages = with pkgs;
    outputs.lib.optionals (config.isDesktop && config.modules.dev.java.ide)
    [ jetbrains.idea-ultimate ];
}

