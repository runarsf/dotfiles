{
  config,
  outputs,
  pkgs,
  ...
}: let
  self = config.modules.dev.python;
  packagesFrom = pythonVersion:
    outputs.lib.getAttr "${pythonVersion}Packages" pkgs;
in
  outputs.lib.mkModule config ["dev" "python"] {
    options' = with outputs.lib; {
      ide = outputs.lib.mkEnableOption "Enable Python IDE";
      packageName = mkOption {
        type = types.enum ["python311" "python312"];
        description = "The package name of the python version to use";
        default = "python311";
      };
      packages = mkOption {
        type = types.listOf types.package;
        default = [];
      };
      presets = {
        math.enable = mkEnableOption "Enable math-related packages";
        jupyter.enable = mkEnableOption "Enable jupyter-related packages";
      };
    };

    config = {
      modules.dev.python.packages = with packagesFrom self.packageName;
        [
          pip
          pydantic
          python-pam
          requests
          pygobject3
          mypy
        ]
        ++ outputs.lib.optionals self.presets.math.enable [
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
        ++ outputs.lib.optionals self.presets.jupyter.enable [
          ipykernel
          jupyterlab
          nbformat
          jupyter-client
          jupyter-core
          notebook
        ]
        ++ outputs.lib.optionals (config.isDesktop && self.ide)
        [jetbrains.pycharm-professional];

      home.packages = with pkgs; [
        poetry
        stdenv.cc.cc.lib
        taglib
        openssl
        libxml2
        libxslt
        libzip
        zlib
        ruff

        (let
          python = outputs.lib.getAttr self.packageName pkgs;
        in (python.withPackages (ps: self.packages)))

        # This uses strings to define packages
        # (python311.withPackages (ps:
        #   (map (pkg:
        #     let pkgPath = outputs.lib.splitString "." pkg;
        #     in outputs.lib.getAttrFromPath pkgPath ps))
        #   (builtins.filter (pkg: outputs.lib.isString pkg)
        #     self.packages)))
      ];
    };
  }
