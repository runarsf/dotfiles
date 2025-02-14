{
  pkgs,
  outputs,
  ...
}: {
  home = {
    sessionVariables = {
      CHROME_EXECUTABLE = "${pkgs.chromium}/bin/chromium";
    };

    packages = with pkgs; [
      gnumake
      just
    ];
  };
}
