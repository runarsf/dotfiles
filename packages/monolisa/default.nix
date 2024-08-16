# pkgs.stdenvNoCC.mkDerivation {
#         name = "gillsans-font";
#         dontConfigue = true;
#         src = pkgs.fetchzip {
#           url =
#             "https://freefontsvault.s3.amazonaws.com/2020/02/Gill-Sans-Font-Family.zip";
#           sha256 = "sha256-YcZUKzRskiqmEqVcbK/XL6ypsNMbY49qJYFG3yZVF78=";
#           stripRoot = false;
#         };
#         installPhase = ''
#           mkdir -p $out/share/fonts
#           cp -R $src $out/share/fonts/opentype/
#         '';
#         meta = { description = "A Gill Sans Font Family derivation."; };
#       };

{ stdenvNoCC }:
stdenvNoCC.mkDerivation (finalAttrs: {
  name = "MonoLisa-font";
  dontConfigure = true;
  src = 
  # nativeBuildInputs = [ makeWrapper ];

})
