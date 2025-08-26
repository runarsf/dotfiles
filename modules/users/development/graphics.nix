{
  pkgs,
  name,
  outputs,
  config,
  ...
}:
outputs.lib.mkModule config ["dev" "graphics"]
{
  home.packages = with pkgs; [
    # https://nixos.wiki/wiki/OpenGL
    glfw
    glm
  ];

  modules.dev.python.packages = with pkgs.python311Packages; [
    glad
  ];
}
