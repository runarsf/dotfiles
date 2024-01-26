{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ pavucontrol qpwgraph ];

  sound.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    audio.enable = true;
    pulse.enable = true;
    # config.pipewire = { "context.properties" = { default.clock.allowed-rates = [ 44100 48000 ]; }; };
    wireplumber.enable = true;

    # If you want to use JACK applications, uncomment this
    jack.enable = true;
    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    # media-session.enable = true;
  };

  security.rtkit.enable = true;

  hardware.pulseaudio = {
    enable = false;
    # Prevent Spotify from muting when another audio source is running
    extraConfig = "unload-module module-role-cork";
  };
}
