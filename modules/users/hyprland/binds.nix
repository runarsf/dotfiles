{
  config,
  outputs,
  pkgs,
  hypr-gamemode,
  hypr-workspace,
  hypr-snap,
  hypr-move,
  ...
}: let
  inherit (outputs.lib) run runOnce toggle;
in {
  wayland.windowManager.hyprland = with outputs.lib; {
    settings = {
      binds = {
        allow_workspace_cycles = true;
      };
      bind = [
        "SUPER, Return, exec, ${run <| config.modules.terminal.exec []}"
        "SUPER, Q, killactive"
        ''SUPER SHIFT, E, exec, loginctl terminate-user ""''
        "SUPER, E, exec, ${run <| getExe pkgs.nautilus}"
        "SUPER SHIFT, F, togglefloating"
        "SUPER ALT, F, workspaceopt, allfloat"
        "SUPER, F, fullscreen, 0"
        "SUPER, space, fullscreen, 1"
        "SUPER, A, exec, ${./. + /bin/hypr-pin}"
        "ALT, P, exec, ${run ''${getExe pkgs.grim} -g "$(${getExe pkgs.slurp})" - | ${getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${getExe' pkgs.wl-clipboard "wl-copy"}''}"
        "ALT SHIFT, P, exec, ${run ''${getExe pkgs.grim} -g "$(${getExe pkgs.slurp})" - | ${getExe' pkgs.imagemagick "convert"} - -shave 1x1 PNG:- | ${getExe pkgs.swappy} -f -''}"

        "SUPER, left, exec, ${hypr-move} focus l"
        "SUPER, right, exec, ${hypr-move} focus r"
        "SUPER, up, exec, ${hypr-move} focus u"
        "SUPER, down, exec, ${hypr-move} focus d"

        "SUPER SHIFT, TAB, centerwindow"
        "SUPER SHIFT, Return, layoutmsg, swapwithmaster"
        # "SUPER SHIFT, space, layoutmsg, orientationcycle left center"
        # "SUPER, bar, layoutmsg, orientationcycle left right"
        # "SUPER, bar, layoutmsg, swapsplit"
        # "SUPER, O, pseudo"
        # "SUPER, B, exec, hyprctl keyword general:layout master"
        # "SUPER SHIFT, B, exec, hyprctl keyword general:layout dwindle"

        "SUPER, X, exec, ${runOnce <| getExe pkgs.hyprlock}"
        "SUPER, L, exec, ${runOnce <| getExe pkgs.hyprlock}"
        "SUPER, TAB, workspace, previous_per_monitor"

        "SUPER, C, exec, ${getExe pkgs.hyprpicker} -a | tr -d '\\n' | ${getExe' pkgs.wl-clipboard "wl-copy"}"

        "SUPER SHIFT, C, exec, ${hypr-gamemode} toggle"

        "SUPER, mouse_down, workspace, e+1"
        "SUPER, mouse_up, workspace, e-1"
      ];
      binde = let
        wpctl = outputs.lib.getExe' pkgs.wireplumber "wpctl";
        playerctl = outputs.lib.getExe pkgs.playerctl;
        brightnessctl = outputs.lib.getExe pkgs.brightnessctl;
      in [
        "SUPER CTRL, right, resizeactive, 50 0"
        "SUPER CTRL, left, resizeactive, -50 0"
        "SUPER CTRL, up, resizeactive, 0 -50"
        "SUPER CTRL, down, resizeactive, 0 50"

        "SUPER SHIFT, right, exec, ${hypr-move} move r"
        "SUPER SHIFT, left, exec, ${hypr-move} move l"
        "SUPER SHIFT, up, exec, ${hypr-move} move u"
        "SUPER SHIFT, down, exec, ${hypr-move} move d"

        ", XF86AudioRaiseVolume, exec, ${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, ${wpctl} set-volume -l 2.0 @DEFAULT_SINK@ 5%-"
        ", XF86AudioMute, exec, ${wpctl} set-mute @DEFAULT_SINK@ toggle"
        ", XF86AudioMicMute, exec, ${wpctl} set-mute @DEFAULT_SOURCE@ toggle"
        ", XF86AudioPause, exec, ${playerctl} play-pause"
        ", XF86AudioPlay, exec, ${playerctl} play-pause"
        "SHIFT, XF86AudioMute, exec, ${playerctl} play-pause"
        ", XF86AudioNext, exec, ${playerctl} next"
        ", XF86AudioPrev, exec, ${playerctl} previous"
        ", XF86MonBrightnessUp, exec, ${brightnessctl}/bin/brightnessctl set 5%+"
        ", XF86MonBrightnessDown, exec, ${brightnessctl}/bin/brightnessctl set 5%-"
      ];
      bindr = [
        "SUPER CTRL, right, exec, ${hypr-snap}"
        "SUPER CTRL, left, exec, ${hypr-snap}"
        "SUPER CTRL, up, exec, ${hypr-snap}"
        "SUPER CTRL, down, exec, ${hypr-snap}"

        "SUPER SHIFT, right, exec, ${hypr-snap}"
        "SUPER SHIFT, left, exec, ${hypr-snap}"
        "SUPER SHIFT, up, exec, ${hypr-snap}"
        "SUPER SHIFT, down, exec, ${hypr-snap}"

        "SUPER, mouse:272, exec, ${hypr-snap}"
        "SUPER, mouse:273, exec, ${hypr-snap}"
      ];
      bindm = [
        "SUPER, mouse:272, movewindow"
        "SUPER SHIFT, mouse:272, movewindow"
        "SUPER, mouse:273, resizewindow"
        "SUPER SHIFT, mouse:273, resizewindow"
      ];
    };
    extraConfig = ''
      # Passthrough mode (e.g. for VNC)
      bind=SUPER SHIFT,P,submap,passthrough
      submap=passthrough
      bind=SUPER SHIFT,P,submap,reset
      submap=reset

      # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
      ${builtins.concatStringsSep "\n" (
        builtins.genList (
          x: let
            ws = let
              c = (x + 1) / 10;
            in
              toString (x + 1 - (c * 10));
          in ''
            bind = SUPER, ${ws}, exec, ${hypr-workspace} ${toString (x + 1)}
            bind = SUPER SHIFT, ${ws}, movetoworkspacesilent, ${toString (x + 1)}
          ''
        )
        10
      )}
    '';
  };
}
