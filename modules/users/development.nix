{ pkgs, ... }:

{
  imports = [
    ./nix.nix
    ./python.nix
    # ./vscode.nix
    ./neovim.nix
  ];

  home = {
    packages = with pkgs.unstable; [
      helix
      gnumake
      just

      # JavaScript
      nodejs
      yarn
      deno

      # Rust
      cargo
      rustc
      # rustup

      # C/C++
      (with dotnetCorePackages; combinePackages [ sdk_6_0 sdk_7_0 sdk_8_0 ])
      cmake
      gcc

      # Haskell
      ghc
      cabal-install

      # Networking
      nmap

      # Misc
      watchexec
      gitui

      # IDEs
      # TODO isDesktop
      # jetbrains.clion
      # jetbrains.pycharm-professional

      # graphviz
      # pkgs.master.warp-terminal
    ];
  };

  programs.java = {
    enable = true;
    package = pkgs.unstable.jdk21;
  };
}
