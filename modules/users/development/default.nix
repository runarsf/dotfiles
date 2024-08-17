{ pkgs, outputs, ... }:

{
  imports = [
    ./neovim.nix
    ./vscode.nix
    ./nix.nix
    ./qmk.nix
    ./android.nix
    ./python.nix
    ./javascript.nix
    ./rust.nix
    ./c.nix
    ./haskell.nix
    ./java.nix
  ];

  home = {
    sessionVariables = {
      CHROME_EXECUTABLE = "${pkgs.chromium}/bin/chromium";
    };

    packages = with pkgs; [
      gnumake
      just
      ngrok
    ];
  };
}
