{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "wayland" {
  # TODO Does this belong here?
  nixos.services.libinput.enable = true;

  home.packages = with pkgs; [xorg.xeyes];

  # https://discourse.nixos.org/t/home-manager-and-the-mimeapps-list-file-on-plasma-kde-desktops/37694/7
  xdg = {
    # https://github.com/Vladimir-csp/uwsm?tab=readme-ov-file#4-environments-and-shell-profile
    configFile = {
      # "environment.d/20-user-environment.conf" # https://www.freedesktop.org/software/systemd/man/latest/environment.d.html
      "uwsm/env".text = ''
        export NIXOS_OZONE_WL=1
        export MOZ_ENABLE_WAYLAND=1
      '';
    };

    # Don't generate config at the usual place.
    # Allow desktop applications to write their file association
    # preferences to this file.
    configFile."mimeapps.list".enable = false;
    # Home-manager also writes xdg-mime-apps configuration to the
    # "deprecated" location. Desktop applications will look in this
    # list for associations, if no association was found in the
    # previous config file.
    dataFile."applications/mimeapps.list".force = true;
    mimeApps.enable = true;
  };
}
