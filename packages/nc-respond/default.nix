_: {
  perSystem = {pkgs, ...}: let
    inherit (builtins) readFile;
    inherit (pkgs) writeShellApplication;
  in {
    packages.nc-respond = writeShellApplication {
      name = "nc-respond";
      runtimeInputs = with pkgs; [coreutils];
      text = readFile ./bin/nc-respond.sh;
    };
  };
}
