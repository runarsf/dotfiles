{ lib, ... }: {
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) stdenv;

      version = "26.11.4";
    in
    {
      packages.sable = stdenv.mkDerivation {
        pname = "sable";
        inherit version;

        src = pkgs.fetchurl {
          url = "https://git.sable.moe/SableClient/sable-next/releases/download/v${version}/sable-${version}-amd64.deb";
          hash = "sha256-bRMUy1wfha+7SaNxQ+I7sdVI2GP4ilbmZZFf1KukdKY=";
        };

        nativeBuildInputs = with pkgs; [
          dpkg
          autoPatchelfHook
          makeWrapper
          wrapGAppsHook3
        ];

        buildInputs = with pkgs; [
          alsa-lib
          at-spi2-atk
          at-spi2-core
          cairo
          cups
          dbus
          expat
          glib
          gtk3
          libgbm
          libpulseaudio
          libxkbcommon
          libX11
          libXcomposite
          libXdamage
          libXext
          libXfixes
          libXrandr
          libxcb
          nspr
          nss
          pango
          pipewire
          stdenv.cc.cc.lib
          udev
        ];

        # Loaded with dlopen() at runtime, so not picked up from DT_NEEDED.
        runtimeDependencies = with pkgs; [
          (lib.getLib udev)
          libappindicator-gtk3
          libGL
          libnotify
          libsecret
          libuuid
          libXScrnSaver
          libXtst
          wayland
        ];

        dontConfigure = true;
        dontBuild = true;
        dontStrip = true;
        dontWrapGApps = true;

        unpackPhase = ''
          dpkg-deb -x $src .
        '';

        installPhase = ''
          runHook preInstall

          mkdir -p $out/bin $out/lib $out/share
          cp -r opt/Sable $out/lib/sable
          cp -r usr/share/applications usr/share/icons $out/share/

          # Setuid helper can't work in the store; user namespaces are used instead.
          rm -f $out/lib/sable/chrome-sandbox

          runHook postInstall
        '';

        postFixup = ''
          makeWrapper $out/lib/sable/sable $out/bin/sable \
            "''${gappsWrapperArgs[@]}" \
            --prefix PATH : ${lib.makeBinPath [ pkgs.xdg-utils ]} \
            --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations --enable-wayland-ime=true}}"

          substituteInPlace $out/share/applications/moe.sable.client.desktop \
            --replace-fail /opt/Sable/sable $out/bin/sable
        '';

        meta = with lib; {
          description = "A Matrix client";
          homepage = "https://sable.moe";
          license = licenses.agpl3Plus;
          sourceProvenance = with sourceTypes; [ binaryNativeCode ];
          platforms = [ "x86_64-linux" ];
          mainProgram = "sable";
        };
      };
    };
}
