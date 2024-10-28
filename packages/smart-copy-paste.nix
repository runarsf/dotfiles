{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  pname = "smart-copy-paste";
  version = "3.2.1";
  scriptName = "SmartCopyPaste.lua";

  nativeBuildInputs = with pkgs; [ git ];

  src = pkgs.fetchgit {
    url = "https://github.com/Eisa01/mpv-scripts.git";
    rev = "48d68283cea47ff8e904decc9003b3abc3e2123e";
    sha256 = "sha256-WH8b5bU3Gw0IJfJNOxMP3Ts4jYWoCRYkaneKhzN/iOc=";
    sparseCheckout = [ "scripts/SmartCopyPaste.lua" ];
  };
  passthru.updateScript = pkgs.unstableGitUpdater { };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/mpv/scripts/
    cp scripts/SmartCopyPaste.lua $out/share/mpv/scripts/
    runHook postInstall
  '';

  meta = with pkgs.lib; {
    description = "SmartCopyPaste";
    homepage = "https://github.com/Eisa01/mpv-scripts";
    license = licenses.bsd2;
  };
}
