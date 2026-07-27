_: {
  perSystem = { pkgs, ... }: {
    packages = {
      osu-cursor = pkgs.runCommand "osu-cursor" { } ''
        mkdir -p $out/share/icons
        ln -s ${
          pkgs.fetchzip {
            url = "https://github.com/Alleexx129/osu-linux-mouse-pack/releases/download/v1.0.0/Osu-Cursor-Mouse-Pack.tar.gz";
            hash = "sha256-ix5RRAgi5LM4qMxrz1a5RIDarCapHtBDlD1bAhTq6nA=";
          }
        } $out/share/icons/Osu
      '';

      wii-cursor = pkgs.runCommand "wii-cursor" { } ''
        mkdir -p $out/share/icons
        ln -s ${
          pkgs.fetchzip {
            url = "https://github.com/Pigamer37/Wii-Cursors/releases/download/0.1.1/Wii.tar.xz";
            hash = "sha256-0c/Jrh4DTkonio87uYl79wf5BHowFGHzze/InPkvHkY=";
          }
        } $out/share/icons/Wii
      '';
    };
  };
}
