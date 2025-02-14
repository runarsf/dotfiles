{pkgs ? import <nixpkgs> {}}:
pkgs.stdenv.mkDerivation {
  pname = "copy-paste-url";
  version = "latest";
  scriptName = "copy-paste-url.lua";

  nativeBuildInputs = with pkgs; [git];

  src = pkgs.fetchgit {
    url = "https://github.com/elhirchek/copy-paste-url.git";
    rev = "";
    sha256 = "";
    sparseCheckout = ["copy-paste-url.lua"];
  };
  passthru.updateScript = pkgs.unstableGitUpdater {};

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    cp copy-paste-url.lua $out/share/mpv/scripts/
    runHook postInstall
  '';

  meta = {
    description = "copy-paste-url";
    homepage = "https://github.com/elhirchek/copy-paste-url";
  };
}
