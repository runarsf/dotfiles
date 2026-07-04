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

    config = {
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

  flake.homeModules.myModule = {pkgs, ...}: let
    inherit (pkgs) writeShellApplication;
  in {
    home.packages = [
      (writeShellApplication {
        name = "fix-pipewire";
        text = ''
          set -o xtrace
          set +o errexit

          killall -9 pipewire
          while killall easyeffects; do sleep 0.5; done
          while pkill hyprpanel; do sleep 0.5; done
          systemctl --user restart pipewire.service
          systemctl --user restart pipewire.socket
          systemctl --user restart pipewire-pulse.service
          systemctl --user restart pipewire-pulse.socket
          systemctl --user restart easyeffects.service

          printf 'Pipewire-related services restarted, consider rebooting.\n'
        '';
      })
    ];
  };
}
