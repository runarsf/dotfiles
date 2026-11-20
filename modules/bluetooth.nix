_: {
  flake.nixosModules.bluetooth = _: {
    hardware.bluetooth = {
      enable = true;
      powerOnBoot = false;

      settings.General = {
        experimental = true; # show battery

        # https://www.reddit.com/r/NixOS/comments/1ch5d2p/comment/lkbabax/
        # for pairing bluetooth controller
        Privacy = "device";
        JustWorksRepairing = "always";
        Class = "0x000100";
        FastConnectable = true;
      };
    };

    services.blueman.enable = true;
  };

  flake.homeModules.bluetooth = _: {
    services.blueman-applet.enable = true;
  };
}
