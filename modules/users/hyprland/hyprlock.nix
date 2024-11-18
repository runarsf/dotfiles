{ config, pkgs, outputs, ... }:

outputs.lib.mkDesktopModule config "hyprlock" {
  nixos.security.pam.services.hyprlock = { };

  programs.hyprlock = {
    enable = true;
    package = pkgs.unstable.hyprlock;

    settings = {
      background = {
        color = "rgba(25, 20, 20, 1.0)";

        blur_passes = 2;
        blur_size = 3;
        noise = 1.17e-2;
        contrast = 0.8916;
        brightness = 0.8172;
        vibrancy = 0.1696;
        vibrancy_darkness = 0.0;
      };

      input-field = {
        monitor = "";
        size = "200, 30";
        outline_thickness = 3;
        dots_size = 0.33;
        dots_spacing = 0.15;
        dots_center = false;
        outer_color = "rgb(151515)";
        inner_color = "rgb(200, 200, 200)";
        font_color = "rgb(10, 10, 10)";
        fade_on_empty = true;
        placeholder_text = "";
        hide_input = false;

        position = "0, -25";
        halign = "center";
        valign = "center";
      };

      label = [
        {
          monitor = "";
          text = "$TIME";
          color = "rgba(200, 200, 200, 1.0)";
          font_size = 80;
          font_family = "CaskaydiaCove NF Bold";

          position = "0, 150";
          halign = "center";
          valign = "center";
        }
        {
          monitor = "";
          text = "Hi there, $USER";
          color = "rgba(200, 200, 200, 1.0)";
          font_size = "18";
          font_family = "CaskaydiaCove NF";

          position = "0, 10";
          halign = "center";
          valign = "center";
        }
      ];
    };
  };
}
