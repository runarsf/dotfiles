{
  config,
  pkgs,
  outputs,
  ...
}:

outputs.lib.mkModule config "wayland" {
  # TODO These should be set *by* hyprland, not here, so you can use x-wms still
  home.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    MOZ_ENABLE_WAYLAND = "1";
    # QT_QPA_PLATFORM = "wayland";
    # LIBSEAT_BACKEND = "logind";
    # XDG_SESSION_TYPE = "wayland";
    # WLR_NO_HARDWARE_CURSORS = "1";
    # _JAVA_AWT_WM_NONREPARENTING = "1";
    # GDK_SCALE = "2";
    # ELECTRON_OZONE_PLATFORM_HINT = "wayland";
  };
  # NOTE This will break stuff if there is a non-wayland user on the same machine,
  #  but application launchers need this.
  # nixos.environment.sessionVariables = waylandEnv;

  nixos.services.libinput.enable = true;

  home.packages = with pkgs; [ xorg.xeyes ];

  # https://discourse.nixos.org/t/home-manager-and-the-mimeapps-list-file-on-plasma-kde-desktops/37694/7
  xdg = {
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
