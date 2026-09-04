{ lib, ... }: {
  perSystem =
    { pkgs, ... }:
    let
      inherit (pkgs) stdenv;
    in
    let
      version = "26.0-beta.1";

      # Zaku (https://github.com/buildzaku/zaku) is a Rust desktop app built on
      # Zed's GPUI framework. It vendors large parts of Zed's own crates and
      # several git-pinned forks, so building from source is roughly as
      # involved as packaging Zed itself. Upstream publishes prebuilt Linux
      # tarballs per release, so we just fetch and patch those instead.
      sources = {
        x86_64-linux = {
          url = "https://github.com/buildzaku/zaku/releases/download/${version}/Zaku-${version}-linux-x86_64.tar.gz";
          hash = "sha256-5dubXmZWXsQXOo1rv+EKIRkIhxxZv1GPgRMYwCfnfF4=";
        };
        aarch64-linux = {
          url = "https://github.com/buildzaku/zaku/releases/download/${version}/Zaku-${version}-linux-aarch64.tar.gz";
          hash = "sha256-pZwKBm+BKNPxybY3M3hOSc1eI8R3i1ZnC4rvrgLF2Qc=";
        };
      };

      source =
        sources.${stdenv.hostPlatform.system}
          or (throw "zaku: no prebuilt release for ${stdenv.hostPlatform.system}");
    in
    {
      packages.zaku = stdenv.mkDerivation {
        pname = "zaku";
        inherit version;

        src = pkgs.fetchurl { inherit (source) url hash; };

        nativeBuildInputs = with pkgs; [
          autoPatchelfHook
          makeBinaryWrapper
        ];

        # For libgcc_s.so.1; libc/libm/the dynamic linker are picked up
        # automatically from stdenv's glibc.
        buildInputs = [ stdenv.cc.cc.lib ];

        sourceRoot = "zaku.app";

        dontConfigure = true;
        dontBuild = true;

        installPhase = ''
          runHook preInstall

          mkdir -p $out
          cp -r libexec lib $out/

          install -Dm644 share/icons/hicolor/512x512/apps/zaku.png \
            $out/share/icons/hicolor/512x512/apps/zaku.png
          install -Dm644 share/applications/dev.zaku.Zaku.desktop \
            $out/share/applications/dev.zaku.Zaku.desktop

          # Vulkan/EGL/Wayland client libs are dlopen'd at runtime, so
          # autoPatchelfHook (which only patches DT_NEEDED entries) can't see
          # them; make sure the loader finds them anyway. The bundled
          # libxkbcommon was built expecting /usr/share/X11/xkb, which doesn't
          # exist on NixOS, so point it at xkeyboard-config explicitly.
          makeWrapper $out/libexec/zaku $out/bin/zaku \
            --prefix PATH : ${lib.makeBinPath [ pkgs.git ]} \
            --suffix LD_LIBRARY_PATH : ${
              lib.makeLibraryPath [
                pkgs.vulkan-loader
                pkgs.wayland
                pkgs.libGL
              ]
            } \
            --set-default XKB_CONFIG_ROOT ${pkgs.xkeyboard_config}/share/X11/xkb

          runHook postInstall
        '';

        meta = {
          description = "Fast, open-source API client with fangs";
          homepage = "https://github.com/buildzaku/zaku";
          changelog = "https://github.com/buildzaku/zaku/releases/tag/${version}";
          license = lib.licenses.agpl3Plus;
          platforms = [
            "x86_64-linux"
            "aarch64-linux"
          ];
          mainProgram = "zaku";
          sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
        };
      };
    };
}
