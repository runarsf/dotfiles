{
  pkgs,
  inputs,
  outputs,
  config,
  ...
}:
outputs.lib.mkEnabledModule config "shell-utils" {
  programs.fzf.enable = true;

  home = {
    packages = with pkgs; [
      inputs.alien.packages.${system}.nix-alien
      bat
      sad
      socat
      btop
      yt-dlp
      appimage-run
      nmap
      dig
      eza
      tldr
      bc
      yq
      jq
      fx
      moreutils
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

    file.".bcrc".text = ''
      define pow(a, b) {
        if (scale(b) == 0) {
          return a ^ b;
        }
        return e(b*l(a));
      }
    '';

    shellAliases = {
      ls = "EZA_ICON_SPACING=2 ${pkgs.eza}/bin/eza -l -F -g -a --group-directories-first --no-time --git";
      grep = "grep --color=always";
      cat = "${pkgs.bat}/bin/bat";
      develop = outputs.lib.mkForce "nix develop --command zsh";
      docker-compose = "docker compose";
      dkcUf = "docker compose up -d --force-recreate";
    };
  };

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
}
