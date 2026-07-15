{
  self,
  inputs,
  lib',
  ...
}: {
  flake.homeModules.python = {
    config,
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mkEnableOption mkOption optionals toList types;
    inherit (lib') flatten;
    inherit (lib'.matching) matchString matchStringList;

    cfg = config.features.python;
  in {
    options.features.python = with types; {
      ide = mkEnableOption "Python IDE";
      version = mkOption {
        type = enum ["3.11" "3.12" "3.13"];
        default = "3.11";
      };
      bundles = mkOption {
        type = let
          presets = enum ["math" "jupyter"];
        in
          either presets (listOf presets);
        default = [];
        apply = toList;
      };
    };

    config = {
      home.packages = with pkgs; [
        stdenv.cc.cc.lib
        ruff

        (let
          python = matchString cfg.version [
            ["3.11" python311]
            ["3.12" python312]
            ["3.13" python313]
          ];
        in (python.withPackages [
            requests
          ]
          ++ flatten (matchStringList cfg.bundles [
            [
              "math"
              [
                pandas
                pyglet
                scipy
                numpy
                mpmath
                sympy
                pyopengl
                pyopengl-accelerate
                numba
                llvmlite
                matplotlib
              ]
            ]
            [
              "jupyter"
              [
                ipykernel
                jupyterlab
                nbformat
                jupyter-client
                jupyter-core
                notebook
              ]
            ]
          ])
          ++ optionals cfg.ide [jetbrains.pycharm-professional]))
      ];
    };
  };
}
