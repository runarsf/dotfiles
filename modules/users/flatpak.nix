{ config, inputs, outputs, ... }:

outputs.lib.mkModule config "flatpak" {
  nixos = {
    imports = [ inputs.nix-flatpak.nixosModules.nix-flatpak ];
    services.flatpak = {
      enable = true;
      update = {
        onActivation = true;
        auto = {
          enable = true;
          onCalendar = "daily";
        };
      };
    };
  };
}
