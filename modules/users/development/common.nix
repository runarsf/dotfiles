{ pkgs, outputs, ... }:

{
  home = {
    sessionVariables = {
      CHROME_EXECUTABLE = "${pkgs.chromium}/bin/chromium";
    };

    packages = with pkgs; [
      # unstable.zed-editor
      gnumake
      just
      ngrok
    ];
  };
}
