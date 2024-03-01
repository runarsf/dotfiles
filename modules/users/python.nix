{ pkgs, ... }:

let
  python-packages = p:
    with p; [
      python-pam
      requests
      pandas
      pyglet
      scipy
      numpy
      mpmath
      sympy
      ipykernel
      matplotlib
      jupyterlab
      pyzmq
      venvShellHook
      pip

      jupyter-client
      jupyter-core
      notebook
      ipykernel
      cairosvg
      pnglatex
      plotly
      pyperclip
      nbformat
    ];

in {
  home.packages = with pkgs; [
    poetry

    stdenv.cc.cc.lib
    taglib
    openssl
    git
    libxml2
    libxslt
    libzip
    zlib
    (python311.withPackages python-packages)
  ];
}
