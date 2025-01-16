{ config, pkgs, outputs, ... }:

# NOTE You can use pw-top to see pipewire latency, useful for easyeffects

outputs.lib.mkDesktopModule' config "pipewire" (with outputs.lib; {
  lowLatency = mkEnableOption "Enable low latency mode for Pipewire";
}) {
  nixos = {
    environment.systemPackages = with pkgs; [ pwvucontrol qpwgraph ];

    services.pipewire = {
      enable = true;
      audio.enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      wireplumber.enable = true;

      extraConfig.pipewire = {
        "90-sample-rates" = {
          context.properties = outputs.lib.mkDefault {
            default.clock.allowed-rates = [ 44100 48000 96000 ];
            default.clock.quantum = 32;
            default.clock.min-quantum = 32;
            default.clock.max-quantum = 1024;
            link.max-buffers = 64;
          };
        };
        "92-low-latency" = outputs.lib.mkIf config.modules.pipewire.lowLatency {
          context.properties = {
            default.clock.rate = 48000;
            default.clock.quantum = 32;
            default.clock.min-quantum = 32;
            default.clock.max-quantum = 32;
          };
        };
      };
    };

    services.pulseaudio = {
      enable = false;

      # Prevent Spotify from muting when another audio source is running
      extraConfig = "unload-module module-role-cork";
    };
    security.rtkit.enable = true;

    # Fix for pipewire-pulse breaking recently
    systemd.user.services.pipewire-pulse.path = [ pkgs.pulseaudio ];
  };
}
