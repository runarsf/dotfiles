{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  pname = "polycat";
  version = "1.2.0";

  nativeBuildInputs = with pkgs; [ cmake git ];

  src = pkgs.fetchgit {
    url = "https://github.com/2IMT/polycat.git";
    rev = "0c836d592591ae4c1622e7fa51c0f86fbc4a9941";
    # TODO hash is drunk or smth idfk
    sha256 = "sha256-y3BFxXFE04DQImWikcGDe33Y1KQ/Vy1IxWLZlHCxX/E=";
    fetchSubmodules = true;
    deepClone = true;
  };

  buildPhase = ''
    cmake -DCMAKE_BUILD_TYPE=RELEASE .
    cmake --build .
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin/
    cp polycat $out/bin/
    install -Dm644 -t $out/share/fonts/truetype/ $src/res/*.ttf
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "Polycat";
    homepage = "https://github.com/2IMT/polycat";
    license = licenses.mit;
    mainProgram = "polycat";
  };
}
