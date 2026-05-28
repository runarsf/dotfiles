{
  self,
  inputs,
  ...
}: {
  flake.homeModules.vicinae = {
    config,
    lib,
    pkgs,
    ...
  }: let
    sys = pkgs.stdenv.hostPlatform.system;
    lua = lib.generators.mkLuaInline;
    run = program: "uwsm app -- ${program}";
    onStart = cmd: {
      _args = ["hyprland.start" (lua "function() hl.exec_cmd(${builtins.toJSON cmd}) end")];
    };
  in {
    imports = [inputs.vicinae.homeManagerModules.default];

    services.vicinae = {
      enable = true;
      systemd = {
        enable = true;
        autoStart = true;
        environment.USE_LAYER_SHELL = 1;
      };
      settings = {
        "$schema" = "https://vicinae.com/schemas/config.json";
        close_on_focus_loss = true;
        consider_preedit = true;
        pop_to_root_on_close = true;
        favicon_service = "twenty";
        search_files_in_root = true;
        providers."@sovereign/vicinae-extension-awww-switcher-0".preferences = {
          wallpaperPath = "${config.home.homeDirectory}/Pictures/Wallpapers";
          transitionDuration = 1;
        };
      };
      extensions = with inputs.vicinae-extensions.packages.${sys}; [
        bluetooth
        nix
        power-profile
        awww-switcher
        it-tools
        ssh
        color-converter
      ];
    };

    home.packages = [
      inputs.awww.packages.${sys}.awww
      pkgs.matugen
    ];

    wayland.windowManager.hyprland.settings = {
      bind = [{
        _args = [
          "SUPER + D"
          (lua "hl.dsp.exec_cmd(${builtins.toJSON (run "vicinae toggle")})")
          {}
        ];
      }];
      layer_rule = [
        {match = {namespace = "vicinae";}; blur = true;}
        {match = {namespace = "vicinae";}; no_anim = true;}
      ];
      on = [
        (onStart (run (lib.getExe' inputs.awww.packages.${sys}.awww "awww-daemon")))
      ];
    };
  };
}
