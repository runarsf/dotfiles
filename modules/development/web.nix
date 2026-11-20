_: {
  flake.homeModules.web =
    {
      pkgs,
      lib,
      ...
    }:
    {
      home.packages =
        (with pkgs; [
          nodejs
          bun
          typescript-language-server
        ])
        ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux (
          with pkgs;
          [
            chromium
          ]
        );

      home.sessionVariables = {
        # Keep `npm install -g` inside the user profile instead of trying to
        # write to the read-only Nix store.
        NPM_CONFIG_PREFIX = "$HOME/.npm-global";
      }
      // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
        CHROME_BIN = "${pkgs.chromium}/bin/chromium";
        CHROMIUM_BIN = "${pkgs.chromium}/bin/chromium";
        PUPPETEER_SKIP_DOWNLOAD = "1";
        PUPPETEER_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";
        PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
      };

      home.sessionPath = [ "$HOME/.npm-global/bin" ];
    };
}
