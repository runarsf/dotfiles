{lib, ...}: let
  baseDomain = domains: subdomain:
    lib.findFirst
      (d: lib.hasSuffix ".${d}" subdomain || subdomain == d)
      subdomain
      domains;
in {
  libExtensions = [
    {
      nginx = {
        inherit baseDomain;
        # Returns { forceSSL; sslCertificate; sslCertificateKey } for a wildcard cert.
        # Merge with your locations: wildcardVhost bd // { locations = ...; }
        wildcardVhost = bd: {
          forceSSL = true;
          sslCertificate = "/var/lib/acme/${bd}/cert.pem";
          sslCertificateKey = "/var/lib/acme/${bd}/key.pem";
        };
      };
    }
  ];
}
