{ pkgs, inputs, ... }:

{
  # NOTE Stylix requires both nixos and home-manager to have the same stateVersion
  # FIXME https://github.com/danth/stylix/issues/241
  imports = [
    inputs.stylix.nixosModules.stylix
  ];

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";

  # TODO For some reason this is mandatory
  stylix.image = ../../users/runar/wallpaper.jpg;
}
