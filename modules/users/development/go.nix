{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config ["dev" "go"] {
  options' = {
    ide = outputs.lib.mkEnableOption "Enable Go IDE";
  };

  config = {
    nixpkgs.overlays = [
      (_: prev: {
        gorun = pkgs.buildGoModule {
          name = "gorun";
          vendorHash = null;
          src = pkgs.fetchFromGitHub {
            owner = "erning";
            repo = "gorun";
            rev = "02445e31634ff49849d1afa7401c34448e3ff64b";
            sha256 = "sha256-2Z5kz6w8k7Pa2U5/p3BZZC7rM6lRvbYnIVnYrcoCEyU=";
          };
        };
      })
    ];
    home.packages = with pkgs;
      [go gorun]
      ++ outputs.lib.optionals (config.isDesktop && config.modules.dev.go.ide)
      [jetbrains.goland];
  };
}
