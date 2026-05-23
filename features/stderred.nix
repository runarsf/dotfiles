{self, ...}: {
  flake.homeModules.stderred = {pkgs, ...}: {
    home.sessionVariablesExtra = ''
      # https://unix.stackexchange.com/a/26776
      # https://unix.stackexchange.com/a/53587
      export STDERRED_BLACKLIST="^(niks|nix|nh|ssh|gitui|vim|neovim|just|yazi)$"
      export LD_PRELOAD="${self.packages.${pkgs.stdenv.hostPlatform.system}.stderred}/lib/libstderred.so''${LD_PRELOAD:+:$LD_PRELOAD}"
    '';
  };
}
