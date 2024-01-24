{ pkgs, outputs, config, ... }:

{
  options.wallpaper = outputs.lib.mkOption {
    default = "";
    type = outputs.lib.types.path;
    description = ''
      Wallpaper path
    '';
  };

  config = {
    home.file.".local/bin/wallpaper.sh".source = let
      script = pkgs.writeShellScriptBin "wallpaper.sh" ''
        ${pkgs.unstable.swww}/bin/swww init &
        sleep 1
        ${pkgs.unstable.swww}/bin/swww img --transition-type any --transition-step 100 --transition-fps 60 ${config.wallpaper}
      '';
    in "${script}/bin/wallpaper.sh";

    wayland.windowManager.hyprland.settings.exec = [ "${config.home.homeDirectory}/.local/bin/wallpaper.sh" ];
  };

}
