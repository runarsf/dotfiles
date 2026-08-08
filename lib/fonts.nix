_: let
  flakeIgnore = [
    "E302"
    "W293"
    "E226"
    "E305"
    "E265" # from nix-shell shebang
    "E501" # line too long (82 > 79 characters)
    "F403" # ‘from module import *’ used; unable to detect undefined names
    "F405" # name may be undefined, or defined from star imports: module
  ];

  mkPythonTool = pkgs: name: libraries: src:
    pkgs.lib.getExe (pkgs.writers.writePython3Bin name
      {
        inherit libraries flakeIgnore;
      } (builtins.readFile src));
in {
  libExtensions = [
    {
      fonts = {
        resizebdf = pkgs:
          mkPythonTool pkgs "resizebdf" (with pkgs.python3Packages; [numpy]) ../packages/fonts/bin/resize_bdf.py;
        resizettf = pkgs:
          mkPythonTool pkgs "resizettf" (with pkgs.python3Packages; [fonttools]) ../packages/fonts/bin/resize_ttf.py;
        renamettf = pkgs:
          mkPythonTool pkgs "renamettf" (with pkgs.python3Packages; [fonttools]) ../packages/fonts/bin/rename_ttf.py;
      };
    }
  ];
}
