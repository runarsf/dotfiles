{ pkgs, inputs, ... }:

{
  # NOTE Stylix requires both nixos and home-manager to have the same stateVersion
  # FIXME https://github.com/danth/stylix/issues/241
  imports = [
    inputs.stylix.homeManagerModules.stylix
  ];

  stylix = {
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";

    # NOTE For some reason this is mandatory
    image = ../../users/runar/wallpaper.jpg;
  };
}
