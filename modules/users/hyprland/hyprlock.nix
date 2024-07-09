{ pkgs, ... }:

{
  programs.hyprlock = {
    enable = true;

    # TODO Configure Hyprlock
    # settings = {
    #   background = {
    #     path = ${./. + /lock.png;
    #     color = rgba(25, 20, 20, 1.0)

    #     # all these options are taken from hyprland, see https://wiki.hyprland.org/Configuring/Variables/#blur for explanations
    #     blur_passes = 4 # 0 disables blurring
    #     blur_size = 7
    #     noise = 0.0117
    #     contrast = 0.8916
    #     brightness = 0.8172
    #     vibrancy = 0.1696
    #     vibrancy_darkness = 0.0
    #   }

    #   input-field {
    #     monitor =
    #     size = 200, 30
    #     outline_thickness = 3
    #     dots_size = 0.33 # Scale of input-field height, 0.2 - 0.8
    #     dots_spacing = 0.15 # Scale of dots' absolute size, 0.0 - 1.0
    #     dots_center = false
    #     outer_color = rgb(151515)
    #     inner_color = rgb(200, 200, 200)
    #     font_color = rgb(10, 10, 10)
    #     fade_on_empty = true
    #     placeholder_text =
    #     hide_input = false

    #     position = 0, -25
    #     halign = center
    #     valign = center
    #   }
    #   label {
    #     monitor =
    #     text = $TIME
    #     color = rgba(200, 200, 200, 1.0)
    #     font_size = 80
    #     font_family = JetBrainsMono NF ExtraBold

    #     position = 0, 150
    #     halign = center
    #     valign = center
    #   }
    #   label {
    #     monitor =
    #     text = Hi there, $USER
    #     color = rgba(200, 200, 200, 1.0)
    #     font_size = 18
    #     font_family = JetBrains Mono Nerd Font

    #     position = 0, 10
    #     halign = center
    #     valign = center
    #   }
    # };
  };
}
