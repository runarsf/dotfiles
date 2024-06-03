{ pkgs, inputs, ... }:

{
  imports = [
    inputs.stylix.nixosModules.stylix
  ];

  stylix.base16Scheme = "${pkgs.base16-schemes}/share/themes/ayu-dark.yaml";

  # TODO For some reason this is mandatory
  stylix.image = ../../users/runar/wallpaper.jpg;
}
