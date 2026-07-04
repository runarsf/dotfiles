{
  self,
  inputs,
  ...
}: {
  flake.homeModules.cli = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    programs.fzf.enable = true;

    home.packages = with pkgs; [
      vim
      git
      wget
      curl
      unzip

      bat
      eza
      ripgrep
      fd
      sad

      btop
      ncdu
      dysk
      killall
      moreutils
      watchexec
      expect

      gay
      blahaj
      krabby
      cowsay

      tldr
      dig
      socat
      nmap

      jq
      yq
      fx
      imagemagick
      bc

      yt-dlp
      appimage-run
      inputs.alien.packages.${system}.nix-alien
    ];

    home.file.".bcrc".text = ''
      define pow(a, b) {
        if (scale(b) == 0) {
          return a ^ b;
        }
        return e(b*l(a));
      }
    '';
  };

  flake.homeModules.stderred = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    home.sessionVariablesExtra = ''
      # https://unix.stackexchange.com/a/26776
      # https://unix.stackexchange.com/a/53587
      export STDERRED_BLACKLIST="^(niks|nix|nh|ssh|gitui|vim|neovim|just|yazi)$"
      export LD_PRELOAD="${self.packages.${system}.stderred}/lib/libstderred.so''${LD_PRELOAD:+:$LD_PRELOAD}"
    '';
  };
}
