{ pkgs, ... }:

{
  imports = [
    ./nix.nix
    ./python.nix
  ];

  home = {
    packages = with pkgs.unstable; [
      # JavaScript
      nodejs
      yarn
      deno

      # Rust
      cargo
      rustc
      # rustup

      # C/C#
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
      jetbrains.clion
    ];
  };

  programs.java = {
    enable = true;
    package = pkgs.unstable.jdk21;
  };
}
