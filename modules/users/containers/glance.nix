{
  config,
  outputs,
  pkgs,
  ...
}: let
  configFile =
    {
      theme = {
        background-color = "50 1 6";
        disable-picker = true;
        custom-css-file = "/custom.css";
      };
      pages = [
        {
          name = "Home";
          width = "slim";
          columns = [
            {
              size = "full";
              widgets = [
                {
                  type = "search";
                  autofocus = true;
                  search-engine = "kagi";
                  placeholder = "Search...";
                }
              ];
            }
          ];
        }
        {
          name = "Overview";
          columns = [
            {
              size = "small";
              widgets = [
                {
                  type = "calendar";
                  first-day-of-week = "monday";
                }
                {
                  type = "rss";
                  limit = 10;
                  collapse-after = 3;
                  cache = "12h";
                  feeds = [
                    {
                      url = "https://selfh.st/rss/";
                      title = "selfh.st";
                      limit = 4;
                    }
                    {url = "https://ciechanow.ski/atom.xml";}
                    {
                      url = "https://www.joshwcomeau.com/rss.xml";
                      title = "Josh Comeau";
                    }
                    {url = "https://samwho.dev/rss.xml";}
                    {
                      url = "https://ishadeed.com/feed.xml";
                      title = "Ahmad Shadeed";
                    }
                  ];
                }
              ];
            }
            {
              size = "full";
              widgets = [
                {
                  type = "group";
                  widgets = [
                    {
                      type = "monitor";
                      cache = "1m";
                      title = "Services";
                      sites = [
                        {
                          title = "Jellyfin";
                          url = "https://jellyfin.runar.ch/";
                          icon = "si:jellyfin";
                        }
                        {
                          title = "Soulseek";
                          url = "https://slskd.runar.ch/";
                          icon = "si:traccar";
                        }
                        {
                          title = "wrtag";
                          url = "https://wrtag.runar.ch/";
                          icon = "si:musicbrainz";
                        }
                        {
                          title = "Immich";
                          url = "https://i.runar.ch/";
                          icon = "si:immich";
                        }
                      ];
                    }
                    {
                      type = "server-stats";
                      servers = [
                        {
                          type = "local";
                          name = "Services";
                        }
                      ];
                    }
                  ];
                }
                {
                  type = "group";
                  widgets = [{type = "hacker-news";} {type = "lobsters";}];
                }
                {
                  type = "group";
                  widgets = [
                    {
                      type = "reddit";
                      subreddit = "technology";
                      show-thumbnails = true;
                    }
                    {
                      type = "reddit";
                      subreddit = "selfhosted";
                      show-thumbnails = true;
                    }
                  ];
                }
              ];
            }
            {
              size = "small";
              widgets = [
                {
                  type = "weather";
                  location = "Gjøvik, Norway";
                  units = "metric";
                  hour-format = "24h";
                }
                {
                  type = "to-do";
                }
                {
                  type = "repository";
                  repository = "integrasjonsprosjekt/memora";
                  pull-request-limit = 5;
                  issues-limit = 5;
                }
              ];
            }
          ];
        }
      ];
    }
    |> pkgs.lib.generators.toYAML {}
    |> builtins.toString
    |> pkgs.writeText "glance.yml";
  customCss =
    ''
      body:has(.nav-item-current[href="/home"]) {
        background-image: url("https://images.unsplash.com/photo-1654132723071-ad55fbb0a90a?ixid=M3wxMTI1OHwwfDF8cmFuZG9tfHx8fHx8fHx8MTc2NDE2NDY0Mnw&ixlib=rb-4.1.0&q=85&w=1920");
        background-color: rgba(0, 0, 0, 0.2);
        background-blend-mode: darken;
        background-repeat: no-repeat;
        background-position: center;
        background-size: cover;
      }

      .footer div:has([href="https://github.com/glanceapp/glance"]) {
        display: none;
      }
    ''
    |> pkgs.writeText "custom.css";
in
  outputs.lib.mkModule config ["containers" "glance"] {
    services.podman.containers = {
      "glance" = outputs.lib.mkContainer config {
        image = "docker.io/glanceapp/glance:latest";
        ports = [
          "6145:8080" # http
        ];
        volumes = [
          "${configFile}:/app/config/glance.yml:ro"
        ];
      };
    };

    nixos = {
      services.nginx.virtualHosts = {
        "dash.${config.modules.nginx.domain}" = {
          forceSSL = true;
          sslCertificate = config.modules.nginx.cert;
          sslCertificateKey = config.modules.nginx.key;
          locations."/" = {
            proxyPass = "http://127.0.0.1:6145";
            proxyWebsockets = true;
          };
          locations."= /custom.css" = {
            alias = customCss;
          };
        };
      };
    };
  }
