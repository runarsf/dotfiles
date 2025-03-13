{
  fetchFromGitHub,
  cmake,
  hyprland,
  hyprlandPlugins,
}:
hyprlandPlugins.mkHyprlandPlugin hyprland {
  pluginName = "hyprscroller";
  version = "master";

  src = fetchFromGitHub {
    owner = "dawsers";
    repo = "hyprscroller";
    rev = "fb3b2ec63c85f22a107bd635890fcb1afc30b01f";
    hash = "";
  };

  nativeBuildInputs = [cmake];

  buildInputs = [];
}
