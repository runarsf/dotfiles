{
  config,
  pkgs,
  outputs,
  name,
  ...
}:
# NOTE You can use pw-top to see pipewire latency, useful for easyeffects
# TODO Make this a nu script
# If pipewire suddenly stops working, run the following commands:
# You might have to reboot with your sound devices disconnected afterwards.
#  $ killall -9 pipewire
#  $ killall easyeffects
#  $ pkill hyprpanel
#  $ systemctl --user restart pipewire.service
#  $ systemctl --user restart pipewire.socket
#  $ systemctl --user restart pipewire-pulse.service
#  $ systemctl --user restart pipewire-pulse.socket
#  $ systemctl --user restart easyeffects.service
outputs.lib.mkDesktopModule config "pipewire"
{
  options' = {
    lowLatency = outputs.lib.mkEnableOption "Enable low latency mode for Pipewire";
  };

  config = {
    home.packages = [
      (pkgs.writeShellApplication {
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
    nixos = {
      environment.systemPackages = with pkgs; [
        pwvucontrol
        pavucontrol
        qpwgraph
      ];

      users.users."${name}".extraGroups = ["pipewire"];

      services.pipewire = {
        enable = true;
        audio.enable = true;
        pulse.enable = true;
        alsa = {
          enable = true;
          support32Bit = true;
        };
        wireplumber = {
          enable = true;
          # extraConfig = {
          #   "disable-suspension" = {
          #     # https://wiki.archlinux.org/title/PipeWire#Noticeable_audio_delay_or_audible_pop/crack_when_starting_playback
          #     "monitor.alsa.rules" = [
          #       {
          #         matches = [
          #           {"node.name" = "~alsa_input.*";}
          #           {"node.name" = "~alsa_output.*";}
          #         ];
          #         actions = {
          #           update-props = {"session.suspend-timeout-seconds" = 0;};
          #         };
          #       }
          #     ];
          #   };
          #   "bluetooth-enhancements" = {
          #     "monitor.bluez.properties" = {
          #       "bluez5.enable-sbc-xq" = true;
          #       "bluez5.enable-msbc" = true;
          #       "bluez5.enable-hw-volume" = true;
          #       "bluez5.roles" = ["hsp_hs" "hsp_ag" "hfp_hf" "hfp_ag"];
          #     };
          #   };
          #   # Meant to fix crackling on shitty hw
          #   "50-alsa-config" = {
          #     "monitor.alsa.rules" = [
          #       {
          #         "matches" = [{"node.name" = "~alsa_output.*";}];
          #         "actions" = {
          #           "update-props" = {
          #             "api.alsa.period-size" = 1024;
          #             "api.alsa.headroom" = 8192;
          #           };
          #         };
          #       }
          #     ];
          #   };
          # };
        };

        # extraConfig = {
        #   pipewire = {
        #     # "90-sample-rates" = {
        #     #   context.properties = outputs.lib.mkDefault {
        #     #     default.clock.allowed-rates = [ 44100 48000 96000 ];
        #     #     default.clock.quantum = 32;
        #     #     default.clock.min-quantum = 32;
        #     #     default.clock.max-quantum = 1024;
        #     #     link.max-buffers = 64;
        #     #   };
        #     # };
        #     "92-low-latency" = outputs.lib.mkIf config.modules.pipewire.lowLatency {
        #       context.properties = {
        #         default.clock.rate = 48000;
        #         default.clock.quantum = 32;
        #         default.clock.min-quantum = 32;
        #         default.clock.max-quantum = 32;
        #       };
        #     };
        #   };
        #   pipewire-pulse."92-low-latency" = outputs.lib.mkIf config.modules.pipewire.lowLatency {
        #     context.modules = [
        #       {
        #         name = "libpipewire-module-protocol-pulse";
        #         args = {
        #           pulse.min.req = "32/48000";
        #           pulse.default.req = "32/48000";
        #           pulse.max.req = "32/48000";
        #           pulse.min.quantum = "32/48000";
        #           pulse.max.quantum = "32/48000";
        #         };
        #       }
        #     ];
        #     stream.properties = {
        #       node.latency = "32/48000";
        #       resample.quality = 1;
        #     };
        #   };
        # };
      };

      services.pulseaudio.enable = false;
      security.rtkit.enable = true;

      # Fix for pipewire-pulse breaking recently
      systemd.user.services.pipewire-pulse.path = [pkgs.pulseaudio];
    };
  };
}
