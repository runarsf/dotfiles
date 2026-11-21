_: {
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) buildGoModule fetchFromGitHub;
    in
    {
      packages.gorun = buildGoModule {
        name = "gorun";
        vendorHash = null;
        src = fetchFromGitHub { # follow:branch master
          owner = "erning";
          repo = "gorun";
          rev = "02445e31634ff49849d1afa7401c34448e3ff64b";
          hash = "sha256-2Z5kz6w8k7Pa2U5/p3BZZC7rM6lRvbYnIVnYrcoCEyU=";
        };
      };
    };
}
