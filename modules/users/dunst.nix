_:

{
  services.dunst = {
    enable = true;
    settings = {
      global = {
        follow = "mouse";
        geometry = "350x60-20+48";
        "indicate_hidden" = "yes";
        shrink = "no";
        transparency = "10";
        "notification_height" = "0";
        "separator_height" = "0";
        padding = "8";
        "horizontal_padding" = "8";
        "frame_width" = "2";
        "separator_color" = "auto";
        sort = "yes";
        "idle_threshold" = "120";
        font = "JetBrains Mono 10";
        "line_height" = "0";
        markup = "full";
        format = ''
          <b>%s</b>
          %b'';
        alignment = "left";
        "show_age_threshold" = "60";
        "word_wrap" = "yes";
        ellipsize = "middle";
        "ignore_newline" = "no";
        "stack_duplicates" = "true";
        "hide_duplicate_count" = "false";
        "show_indicators" = "yes";
        "icon_position" = "left";
        "max_icon_size" = "64";
        "sticky_history" = "yes";
        "history_length" = "20";
        dmenu = "/usr/bin/dmenu -p dunst:";
        browser = "/usr/bin/firefox -new-tab";
        "always_run_script" = "true";
        title = "Dunst";
        class = "Dunst";
        "startup_notification" = "false";
        "force_xinerama" = "false";
      };
      experimental = { "per_monitor_dpi" = "false"; };
      "urgency_low" = {
        timeout = "4";
        background = "#141c21";
        foreground = "#93a1a1";
        "frame_color" = "#141c21";
      };
      "urgency_normal" = {
        timeout = "8";
        background = "#141c21";
        foreground = "#93a1a1";
        "frame_color" = "#141c21";
      };
      "urgency_critical" = {
        timeout = "0";
        background = "#141c21";
        foreground = "#93a1a1";
        "frame_color" = "#EA545C";
      };
    };
  };
}
