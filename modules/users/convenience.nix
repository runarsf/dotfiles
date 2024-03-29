{ pkgs, ... }:

{
  programs.fzf.enable = true;

  home = {
    packages = with pkgs; [
      bat
      btop
      eza
      tldr
      yq
      jq
      fx
      ncdu
      killall
      inotify-tools
      libnotify
      libsixel
      glib
      openssl
      unzip
      ripgrep
      expect
      imagemagick
    ];

    shellAliases = rec {
      ls = "EZA_ICON_SPACING=2 ${pkgs.eza}/bin/eza -l -F -g -a --group-directories-first --no-time --git --total-size";
      sl = "${ls} | rev";
      grep = "grep --color=always";
      cat = "${pkgs.bat}/bin/bat";
    };
  };
}
