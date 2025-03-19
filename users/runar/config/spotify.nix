{
  pkgs,
  inputs,
  outputs,
  ...
}: {
  programs.spicetify = {
    theme = outputs.lib.mkForce inputs.spicetify-nix.legacyPackages.${pkgs.system}.themes.text;
    colorScheme = "custom";
    customColorScheme = let
      c = hex: builtins.substring 1 (builtins.stringLength hex - 1) hex;
      bg = c "#0D1017";
      bg-alt = c "#131721";
      fg = c "#BFBDB6";
      secondary = c "#FFB454";
      secondary-alt = c "#E6B673";
    in {
      text = fg;
      subtext = fg;
      button-text = bg;
      subbutton-text = fg;
      main = bg;
      sidebar = bg;
      player = bg;
      card = bg-alt;
      shadow = bg;
      selected-row = bg-alt;
      sub-button = secondary;
      button = secondary;
      button-active = secondary-alt;
      button-disabled = bg-alt;
      sidebar-button = secondary;
      play-button = secondary;
      tab-active = bg;
      notification = bg;
      notification-error = bg;
      playback-bar = secondary;
      misc = secondary;
    };
  };
}
