{
  pkgs,
  outputs,
  config,
  name,
  ...
}:

let
  # TODO If the profile doesn't exist (firefox not enabled), don't die
  wp = "${config.home.homeDirectory}/.mozilla/firefox/${
    config.programs.firefox.profiles."${name}".path
  }/chrome/wallpaper.jpg";

in
{
  options.wallpaper = outputs.lib.mkOption {
    default = null;
    type = outputs.lib.types.path;
  };

  config = outputs.lib.mkIf (config.wallpaper != null) {
    programs.hyprlock.settings.background.path = (builtins.toString config.wallpaper);
    home.file."${wp}".source = config.wallpaper;

    stylix.image = outputs.lib.mkForce config.wallpaper;

    # programs.firefox.profiles."${name}" = {
    #     # url(about:blank),
    #   userContent = ''
    #     @-moz-document url(about:home),
    #     url(about:newtab),
    #     url(about:privatebrowsing) {
    #       #newtab-window,
    #       html,
    #       body {
    #         background-image: url(file://${wp}) !important;
    #         background-size: cover !important;
    #         background-position: center center !important;
    #         background-repeat: no-repeat !important;
    #       }
    #     }
    #   '';
    # };

    home.file.".local/bin/wallpaper.sh".source =
      let
        script = pkgs.writers.writeBashBin "wallpaper.sh" ''
          ${pkgs.unstable.swww}/bin/swww init &
          sleep 1
          ${pkgs.unstable.swww}/bin/swww img --transition-type any --transition-step 100 --transition-fps 60 ${config.wallpaper}
        '';
      in
      "${script}/bin/wallpaper.sh";

    wayland.windowManager.hyprland.settings.exec = [
      "${config.home.homeDirectory}/.local/bin/wallpaper.sh"
    ];
  };
}
