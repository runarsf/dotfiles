{
  config,
  pkgs,
  inputs,
  outputs,
  ...
}:
outputs.lib.mkDesktopModule config "audiorelay" {
  nixpkgs.overlays = [inputs.stackpkgs.overlays.default];

  nixos = {
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

  home.packages = with pkgs; [stackpkgs.audiorelay];
}
