{ pkgs, ... }:

{
  home = {
    packages = with pkgs; [
      bat
      btop
      eza
      tldr
      yq
      jq
      ncdu
      unzip
      ripgrep
      expect
      imagemagick
    ];

    shellAliases = {
      ls = "EZA_ICON_SPACING=2 ${pkgs.eza}/bin/eza -lFga --hyperlink --group-directories-first --no-time --git --total-size";
      grep = "grep --color=always";
      cat = "${pkgs.bat}/bin/bat";
    };
  };
}
