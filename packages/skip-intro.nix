{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  pname = "skip-intro";
  version = "1.0.0";
  scriptName = "skip-intro.lua";

  nativeBuildInputs = with pkgs; [ git ];

  src = pkgs.fetchgit {
    url = "https://github.com/rui-ddc/skip-intro.git";
    rev = "3a7d2e95d5adfa761e1a25097022403f3b8a0cba";
    sha256 = "sha256-1rxwPiYIj4oxy8piRpiJeWU4b8vCnLCOvxt7uoru5xc=";
    sparseCheckout = [ "skip-intro.lua" ];
  };
  passthru.updateScript = pkgs.unstableGitUpdater { };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    cp skip-intro.lua $out/share/mpv/scripts/
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "skip-intro";
    homepage = "https://github.com/rui-ddc/skip-intro";
    license = licenses.mit;
  };
}
