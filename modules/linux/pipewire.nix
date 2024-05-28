{ pkgs, lib, config, ... }:

# NOTE You can use pw-top to see pipewire latency, useful for easyeffects

{
  options = {
    lowLatency = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable low latency mode for Pipewire";
    };
  };

  config = {
    environment.systemPackages = with pkgs; [
      pavucontrol
      qpwgraph
      wireplumber
    ];

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      audio.enable = true;
      pulse.enable = true;
      wireplumber.enable = true;

      extraConfig.pipewire = {
        "90-sample-rates" = {
          context.properties = {
            default.clock.allowed-rates = [ 44100 48000 ];
          };
        };
        "92-low-latency" = lib.mkIf config.lowLatency {
          context.properties = {
            default.clock.rate = 48000;
            default.clock.quantum = 32;
            default.clock.min-quantum = 32;
            default.clock.max-quantum = 32;
          };
        };
      };
    };

    security.rtkit.enable = true;

    # Prevent Spotify from muting when another audio source is running
    hardware.pulseaudio.extraConfig = "unload-module module-role-cork";
  };
}
