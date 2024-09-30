{
  pkgs,
  outputs,
  config,
  ...
}:

outputs.lib.mkModule config "python" {
  home.packages = with pkgs; [
    poetry

    stdenv.cc.cc.lib
    taglib
    openssl
    libxml2
    libxslt
    libzip
    zlib
    git

    (python311.withPackages (
      ps: with ps; [
        pip
        python-pam
        requests
        pygobject3
        # TODO These are required for swww, put them the appropriate place
        # platformdirs
        # importlib-resources
        # importlib-metadata

        debugpy

        pydantic

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

        ipykernel
        matplotlib
        jupyterlab
        pyzmq
        venvShellHook

        jupyter-client
        jupyter-core
        notebook
        ipykernel
        cairosvg
        pnglatex
        plotly
        pyperclip
        nbformat
      ]
    ))
  ];
}
