{ inputs, ... }:
{
  flake.nixosModules.flatpak =
    { config, lib, ... }:
    {
      imports = [ inputs.nix-flatpak.nixosModules.nix-flatpak ];

      # Flatpak asserts on xdg.portal.enable, which only a desktop brings in.
      config = lib.mkIf (config.host.desktop or true) {
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
    };
}
