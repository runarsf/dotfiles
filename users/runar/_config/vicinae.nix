{ inputs, ... }:
{
  homeModule =
    { pkgs, ... }:
    let
      inherit (pkgs.stdenv.hostPlatform) system;
      inherit (inputs.vicinae.lib.${system}) mkRayCastExtension;
    in
    {
      programs.vicinae = {
        settings.providers = {
          "@luolei/karakeep" = {
            enabled = true;
            preferences = {
              apiUrl = "https://links.runar.ch";
              displayBookmarkPreview = true;
              displayBookmarkStatus = false;
              displayCreatedAt = true;
              displayDescription = true;
              displayNote = false;
              displaySummary = false;
              displayTags = true;
              language = "en";
              linkMainAction = "viewDetail";
              prefillUrlFromBrowser = true;
              showWebsitePreview = true;
              textMainAction = "viewDetail";
              verboseLogging = false;
            };
            entrypoints = {
              backups.enabled = false;
              createList.enabled = false;
              createNote.enabled = false;
              highlights.enabled = false;
              notes.enabled = false;
              stats.enabled = false;
            };
          };
        };
        extensions = [
          (mkRayCastExtension {
            name = "karakeep"; # folder name in raycast/extensions repo
            rev = "c0c2cd66304c102d9e98d4d0d339f316d97ee41d";
            hash = "sha256-0zg7cbnlccWjOyL2Ulh5/gmRkQaHtYKcnwD7aBvMGS8=";
            # karakeep's build script uses `-o dist`, so output lands in dist/ not ~/.config/raycast/extensions/
            installPhase = ''
              runHook preInstall
              mkdir -p $out
              cp -r dist/* $out/
              runHook postInstall
            '';
          })
        ];
      };
    };
}
