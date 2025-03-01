{
  hyprland,
  gcc14,
  fetchFromGitHub,
  stdenvNoCC,
}:
stdenvNoCC.mkDerivation rec {
  name = "hypr-workspace-layouts";
  pname = name;
  src = fetchFromGitHub {
    owner = "zakk4223";
    repo = "hyprWorkspaceLayouts";
    rev = "f86ccdbd75459d6be297e115136158ace8f889de";
    sha256 = "sha256-+fjrxbz1IGgMFfEABlJr7BWYFOJOH1Yy+yGJV8h1d7w=";
  };

  inherit (hyprland) buildInputs;
  nativeBuildInputs = hyprland.nativeBuildInputs ++ [hyprland gcc14];

  dontUseCmakeConfigure = true;
  dontUseMesonConfigure = true;
  dontUseNinjaBuild = true;
  dontUseNinjaInstall = true;

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/lib"
    cp -r *.so "$out/lib/lib${name}.so"

    runHook postInstall
  '';
}
