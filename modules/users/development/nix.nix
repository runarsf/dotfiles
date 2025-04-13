{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "nix" {
  # TODO nix dev alias to nix develop --command zsh
  home.sessionVariables = {
    FLAKE = "${config.home.homeDirectory}/.config/nixos";
  };

  programs = {
    nix-index-database.comma.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      config.global.hide_env_diff = true;
    };
  };

  home = {
    packages = with pkgs; [
      nil
      nixfmt-rfc-style
      alejandra
      cached-nix-shell
      deadnix
      statix
      # TODO https://github.com/viperML/nh?tab=readme-ov-file#nixos-module
      nh
      nix-inspect
      nix-output-monitor
      nvd
      nix-tree
      manix
    ];
  };
}
