{pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation {
  pname = "creep2";
  version = "latest";

  src = pkgs.fetchgit {
    url = "https://github.com/raymond-w-ko/creep2.git";
    rev = "69dc0de03d89f31b8074981cec0be45d4aceb245";
    sha256 = "sha256-iVppvnqgui/KUZTqYeEX9Qw8k325ix40+AVG49qmGVw=";
  };

  buildPhase = "true";

  installPhase = ''
    runHook preInstall
    install -Dm644 -t $out/share/fonts/misc/ $src/*.bdf
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "creep2";
    homepage = "https://github.com/raymond-w-ko/creep2";
    license = licenses.mit;
  };
}
