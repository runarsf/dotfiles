{
  pkgs,
  inputs,
  outputs,
  config,
  ...
}:

outputs.lib.mkEnabledModule config "shell-utils" {
  programs.fzf.enable = true;

  nixos = {
    programs.nix-ld.enable = true;
    programs.nix-ld.libraries = with pkgs; [
      ell
      moreutils
      xorg.libpthreadstubs
      fuse
      xorg.libX11
      fontconfig
      freetype
      expat
      libdrm
      xorg.libxcb
      alsa-lib
      e2fsprogs
      libgpg-error
      nss
      sane-backends
      nspr
      zlib
      libglvnd
      qt5.qtbase
      qt5.qtsvg
      qt5.qtdeclarative
      qt5.qtwayland
      qt6.qt5compat
      stdenv.cc.cc
    ];
  };

  home = {
    packages = with pkgs; [
      inputs.alien.packages.${system}.nix-alien
      bat
      sad
      gay
      blahaj
      krabby
      socat
      btop
      yt-dlp
      appimage-run
      nmap
      dig
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
      fd
      expect
      imagemagick
      watchexec
    ];

    shellAliases = rec {
      ls = "EZA_ICON_SPACING=2 ${pkgs.eza}/bin/eza -l -F -g -a --group-directories-first --no-time --git";
      sl = "${ls} | rev";
      grep = "grep --color=always";
      cat = "${pkgs.bat}/bin/bat";
    };
  };
}
