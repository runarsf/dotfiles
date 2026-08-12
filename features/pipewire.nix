_: {
  # NOTE You can use pw-top to see pipewire latency, useful for easyeffects
  flake.nixosModules.pipewire = {
    pkgs,
    lib,
    config,
    ...
  }: let
    inherit (lib) mkEnableOption;
  in {
    options.features.pipewire = {
      lowLatency = mkEnableOption "Enable low latency mode for Pipewire";
    };

    config = lib.mkIf (config.hosts.desktop or true) {
      environment.systemPackages = with pkgs; [
        pwvucontrol
        pavucontrol
        qpwgraph
      ];

      users.groups.pipewire.members = config.primaryUsers;

      services.pipewire = {
        enable = true;
        audio.enable = true;
        pulse.enable = true;
        alsa = {
          enable = true;
          support32Bit = true;
        };
        wireplumber.enable = true;
      };

      services.pulseaudio.enable = false;
      security.rtkit.enable = true;

      # Fix for pipewire-pulse breaking recently
      systemd.user.services.pipewire-pulse.path = [pkgs.pulseaudio];
    };
  };
}
