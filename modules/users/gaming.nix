{ pkgs, ... }:

{
  home.packages = with pkgs; [
    prismlauncher
    # TODO GDLauncher
  ];

  nixos.services.ratbagd.enable = true;
}
