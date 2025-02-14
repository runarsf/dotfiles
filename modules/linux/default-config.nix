{
  outputs,
  pkgs,
  hostname,
  stateVersion,
  ...
}:
with outputs.lib; {
  imports = [
    ./user-nixos-configs.nix
  ];

  system.stateVersion = mkDefault stateVersion;
  networking.hostName = mkDefault hostname;

  environment.systemPackages = with pkgs; [
    vim
    git
    wget
    curl
    niks
  ];

  nix.gc = {
    automatic = mkDefault true;
    dates = mkDefault "weekly";
    options = mkDefault "--delete-older-than 5d";
  };
}
