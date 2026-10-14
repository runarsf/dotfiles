{ lib, ... }: {
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) stdenv;

      version = "2.0.2";

      debs = {
        x86_64-linux = {
          url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${version}/sable-${version}-linux-x86_64.deb";
          hash = "sha256-5+UrRXhE/bRL7wWKpg5kWFifQ4ncCg2DFf3BTI9CGJs=";
        };
        aarch64-linux = {
          url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${version}/sable-${version}-linux-aarch64.deb";
          hash = lib.fakeHash;
        };
      };
    in
    {
      packages.sable = stdenv.mkDerivation {
        pname = "sable";
        inherit version;

        src = pkgs.fetchurl (
          debs.${stdenv.hostPlatform.system}
            or (throw "sable: no prebuilt .deb for ${stdenv.hostPlatform.system}")
        );

        nativeBuildInputs = with pkgs; [
          dpkg # unpack the .deb
          autoPatchelfHook
          makeWrapper
          wrapGAppsHook3
        ];

        buildInputs = with pkgs; [
          alsa-lib
          at-spi2-core
          cups
          dbus
          expat
          fontconfig
          freetype
          gdk-pixbuf
          glib
          gtk3
          libayatana-appindicator
          libdrm
          libgbm # mesa's GBM lib; on older channels use `mesa` instead
          libGL
          libxkbcommon
          libxshmfence
          libX11
          libXcomposite
          libXdamage
          libXext
          libXfixes
          libXi
          libXrandr
          libXrender
          libXtst
          libxcb
          nspr
          nss
          pango
          pipewire
          udev
          wayland
        ];

        dontConfigure = true;
        dontBuild = true;
        dontStrip = true; # matches upstream's !strip; never strip a bundled CEF runtime

        unpackPhase = ''
          dpkg-deb -x $src .
        '';

        installPhase = ''
          runHook preInstall

          mkdir -p $out/bin $out/lib $out/share
          # The app tree (sable-next binary + libcef.so, *.pak, locales/, …)
          # must stay in one directory; move it wholesale.
          cp -r opt/sable-next $out/lib/sable-next
          cp -r usr/share/. $out/share/

          # The setuid chrome-sandbox helper can't work in the Nix store;
          # Chromium falls back to unprivileged user namespaces (default on NixOS).
          rm -f $out/lib/sable-next/chrome-sandbox

          # Replace the deb's `exec /opt/sable-next/sable-next` shim [^f330cc#60-74]
          # with a proper wrapper.
          # CEF dlopen()s these at runtime instead of linking them as DT_NEEDED,
          # so autoPatchelfHook never gives them an rpath even though they're
          # already in buildInputs; without this the window fails to create.
          # desktop-file-utils' update-desktop-database is run by the deep-link
          # plugin's setup hook, which panics (crashing the app) if it's missing.
          makeWrapper $out/lib/sable-next/sable-next $out/bin/sable-next \
            --prefix PATH : ${
              lib.makeBinPath (
                with pkgs;
                [
                  desktop-file-utils
                  xdg-utils
                  xwayland
                ]
              )
            } \
            --prefix LD_LIBRARY_PATH : ${
              lib.makeLibraryPath (
                with pkgs;
                [
                  libX11
                  libxkbcommon
                ]
              )
            }
          ln -s sable-next $out/bin/sable

          # Desktop file: upstream generates it at build time, so its Exec= may be
          # bare, /usr/bin/-based, or /opt/sable-next/-based. Patch what's there,
          # warn (not fail) on what isn't. Icon= may also use an absolute path.
          for f in $out/share/applications/*.desktop; do
            [ -e "$f" ] || continue
            substituteInPlace "$f" \
              --replace-warn /usr/bin/ $out/bin/ \
              --replace-warn /opt/sable-next/ $out/lib/sable-next/ \
              --replace-warn /usr/share/ $out/share/
          done

          runHook postInstall
        '';

        meta = with lib; {
          description = "A Matrix client (Sable v2)";
          homepage = "https://sable.moe";
          license = licenses.agpl3Plus;
          sourceProvenance = with sourceTypes; [ binaryNativeCode ];
          platforms = [
            "x86_64-linux"
            "aarch64-linux"
          ];
          mainProgram = "sable-next";
        };
      };
    };
}
