{inputs, ...}: {
  flake.nixosModules.audiorelay = _: {
    networking.firewall = {
      allowedUDPPorts = [
        59100
        59200
      ];
      allowedTCPPorts = [59100];
    };
    # https://docs.audiorelay.net/instructions/linux/stream-audio-from-your-linux-pc-to-your-phone
    services.pulseaudio.extraConfig = ''
      load-module module-null-sink sink_name=audiorelay-speakers sink_properties=device.description=AudioRelay-Speakers
    '';
  };

  flake.homeModules.audiorelay = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    home.packages = [inputs.stackpkgs.${system}.audiorelay];
  };
}
