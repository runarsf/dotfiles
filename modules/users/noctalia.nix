{
  config,
  inputs,
  outputs,
  pkgs,
  ...
}:
{
  imports = [
    inputs.noctalia.homeModules.default
  ];
}
// outputs.lib.mkDesktopModule config "noctalia" {
  programs.noctalia-shell = {
    enable = true;
    systemd.enable = true;
    # https://docs.noctalia.dev/getting-started/nixos/#config-ref
    settings = {
      wallpaper.enabled = false;
    };
  };
}
