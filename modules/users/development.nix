{ pkgs, ... }:

# TODO languages = [ "python", "node", ... ]

{
  imports = [
    ./nix.nix
    ./python.nix
    # ./vscode.nix
    ./neovim.nix
  ];

  home.sessionVariables = {
    CHROME_EXECUTABLE = "${pkgs.chromium}/bin/chromium";
  };

  home = {
    packages = with pkgs.unstable; [
      # JavaScript
      nodejs
      nodePackages."@angular/cli"
      yarn

      # Rust
      cargo
      rustc

      # C/C++/C#
      (with dotnetCorePackages; combinePackages [ sdk_6_0 sdk_7_0 sdk_8_0 ])
      cmake
      gcc

      # Haskell
      ghc
      cabal-install

      # Flutter
      unstable.flutter
      graphite2
      gtk3

      # Misc
      watchexec
      gnumake
      just

      # Formatters & LSPs
      prettierd

      # IDEs
      # TODO isDesktop
      # jetbrains.clion
      # jetbrains.pycharm-professional
      master.android-studio-full
    ];
  };
  nixpkgs.config.android_sdk.accept_license = true;

  programs.java = {
    enable = true;
    package = pkgs.unstable.jdk21;
  };
}
