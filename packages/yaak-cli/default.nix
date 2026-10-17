{ lib, ... }: {
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) stdenv;

      version = "2026.8.1";

      # Yaak's CLI (https://github.com/mountain-loop/yaak) ships on npm as a
      # thin JS wrapper (@yaakapp/cli) around a per-platform prebuilt binary
      # package (@yaakapp/cli-<platform>). The wrapper just spawns the native
      # binary, so we skip npm/node entirely and patch the binary directly.
      sources = {
        x86_64-linux = {
          url = "https://registry.npmjs.org/@yaakapp/cli-linux-x64/-/cli-linux-x64-${version}.tgz";
          hash = "sha512-F5syueaftcsxftmp8Trhtx1QSYjI5uTzgyl5kUWejRzBAnY5JUnCEL1Q3S5/tXVIFAXEa+Fb221XgdMcdU1KMw==";
        };
        aarch64-linux = {
          url = "https://registry.npmjs.org/@yaakapp/cli-linux-arm64/-/cli-linux-arm64-${version}.tgz";
          hash = "sha512-lBAElnM2Jans7Cg6eH34zVWMB+yEQ3/+THrhbbqdIwcnewCgOQfxFi0IQuGGQmLrADqoYiHaqnHX/x47hzCegg==";
        };
      };

      source =
        sources.${stdenv.hostPlatform.system}
          or (throw "yaak-cli: no prebuilt release for ${stdenv.hostPlatform.system}");
    in
    {
      packages.yaak-cli = stdenv.mkDerivation {
        pname = "yaak-cli";
        inherit version;

        src = pkgs.fetchurl { inherit (source) url hash; };

        nativeBuildInputs = with pkgs; [ autoPatchelfHook ];

        # libdbus for IPC with a running Yaak app, openssl for TLS. libgcc_s,
        # libm, libc and the dynamic linker are picked up automatically from
        # stdenv's glibc.
        buildInputs = [
          stdenv.cc.cc.lib
          pkgs.dbus.lib
          pkgs.openssl
        ];

        sourceRoot = "package";

        dontConfigure = true;
        dontBuild = true;

        installPhase = ''
          runHook preInstall

          install -Dm755 bin/yaak $out/bin/yaak

          runHook postInstall
        '';

        meta = {
          description = "Command-line interface for the Yaak API client";
          homepage = "https://github.com/mountain-loop/yaak";
          license = lib.licenses.mit;
          platforms = [
            "x86_64-linux"
            "aarch64-linux"
          ];
          mainProgram = "yaak";
          sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
        };
      };
    };
}
