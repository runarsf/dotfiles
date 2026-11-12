{ self, lib', ... }:
{
  flake.nixosModules.octoFiesta = lib'.mkService {
    inherit self;

    name = "octoFiesta";
    subdomain = "subsonic";
    port = 5274;
    maxBodySize = "2G";

    module =
      { config, ... }:
      let
        inherit (config.features.containers) dirs user;

        # Points at navidrome when it is on this host too.
        subsonicUrl =
          if config.features.navidrome.enable or false then
            "https://${config.features.navidrome.domain}"
          else
            "";
      in
      {
        sops = {
          secrets.octo_fiesta_deezer_arl = { };

          templates."octo-fiesta-env" = {
            owner = user;
            content = ''
              Deezer__Arl=${config.sops.placeholder.octo_fiesta_deezer_arl}
            '';
          };
        };

        features.containers.units.octo-fiesta = {
          image = "ghcr.io/v1ck3s/octo-fiesta:latest";
          ports = [ "5274:8080" ];
          volumes = [ "${dirs.media}/downloads/music:/app/downloads" ];
          environment = {
            ASPNETCORE_ENVIRONMENT = "Production";
            ASPNETCORE_URLS = "http://+:8080";
            Library__DownloadPath = "/app/downloads";

            # Subsonic server to publish the downloads through
            Subsonic__Url = subsonicUrl;
            # Deezer, Qobuz or SquidWTF
            Subsonic__MusicService = "Deezer";
            # Permanent (kept in the library) or Cache (auto-cleanup)
            Subsonic__StorageMode = "Permanent";
            Subsonic__CacheDurationHours = "1";
            Subsonic__EnableExternalPlaylists = "false";
            Subsonic__PlaylistsDirectory = "playlists";
            # All, ExplicitOnly or CleanOnly
            Subsonic__ExplicitFilter = "All";
            # Track or Album
            Subsonic__DownloadMode = "Album";
            Subsonic__AutoUpgradeQuality = "false";
            Subsonic__DisableLibraryScan = "false";

            # FLAC, MP3_320 or MP3_128
            Deezer__Quality = "FLAC";

            # Only used when Subsonic__MusicService is SquidWTF
            SquidWTF__Source = "Qobuz";
            # Qobuz: 27 (24/192), 7 (24/96), 6 (16/44), 5 (MP3 320)
            SquidWTF__Quality = "6";
            SquidWTF__InstanceTimeoutSeconds = "5";
          };
          environmentFiles = [ config.sops.templates."octo-fiesta-env".path ];
        };
      };
  };
}
