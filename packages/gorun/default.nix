_: {
  perSystem = {pkgs, ...}: let
    inherit (pkgs) buildGoModule fetchFromGitHub;
  in {
    packages.gorun = buildGoModule {
      name = "gorun";
      vendorHash = null;
      src = fetchFromGitHub {
        owner = "erning";
        repo = "gorun";
        rev = "02445e31634ff49849d1afa7401c34448e3ff64b";
        sha256 = "sha256-2Z5kz6w8k7Pa2U5/p3BZZC7rM6lRvbYnIVnYrcoCEyU=";
      };
    };
  };
}
