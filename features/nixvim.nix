{
  inputs,
  lib',
  ...
}: {
  flake.homeModules.nixvim = lib'.mkFeature "nixvim" (
    {
      lib,
      pkgs,
      osConfig,
      ...
    }: let
      inherit (pkgs.stdenv.hostPlatform) system;

      nixvim = inputs.nixvim.packages."${system}".default;
      nixvim' = lib.getExe nixvim;
      aliases = {
        vim = "${nixvim'}";
      };
    in {
      home = {
        packages =
          [
            nixvim
          ]
          ++ lib.optionals (osConfig.host.desktop or true) [
            inputs.nixvim.packages."${system}".neovide
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
