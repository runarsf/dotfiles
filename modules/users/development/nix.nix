{
  config,
  pkgs,
  outputs,
  inputs,
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

  nixpkgs.overlays = [
    (_: _: {
      alejandra = inputs.alejandra.packages.${pkgs.stdenv.hostPlatform.system}.default;
    })
  ];

  home = {
    packages = with pkgs; [
      alejandra
      nixd
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
