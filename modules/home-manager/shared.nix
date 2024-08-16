# Modules shared across NixOS, Darwin, and standalone Home Configurations

{ inputs, outputs, system, hostname, ... }:

outputs.lib.mkFor system hostname {
  common = {
    imports = [
      # inputs.sops-nix.homeManagerModules.sops
      inputs.nix-index-database.hmModules.nix-index
      ../nix/nixpkgs.nix
      ./system-config-support.nix
    ];
    options.isDesktop = outputs.lib.mkEnableOption "Override isDesktop";
  };

  systems.darwin.imports = [ inputs.mac-app-util.homeManagerModules.default ];
}
