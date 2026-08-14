{
  inputs,
  lib',
  ...
}: {
  flake.homeModules.nixvim = lib'.mkFeature "nixvim" (
    {
      lib,
      pkgs,
      ...
    }: let
      nixvim = inputs.nixvim.packages."${pkgs.stdenv.hostPlatform.system}".default;
      nixvim' = lib.getExe nixvim;
      aliases = {
        vim = "${nixvim'}";
      };
    in {
      home = {
        packages = [
          nixvim
        ];
        sessionVariables = {
          EDITOR = "${nixvim'}";
          GIT_EDITOR = "${nixvim'}";
          VISUAL = "${nixvim'}";
          DIFFPROG = "${nixvim'} -d";
          MANPAGER = "${nixvim'} +Man!";
          MANWIDTH = 999;
        };
        shellAliases = aliases;
      };

      programs.nushell.shellAliases = {
        inherit aliases;
      };
    }
  );
}
