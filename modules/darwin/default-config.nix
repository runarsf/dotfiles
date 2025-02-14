{
  outputs,
  pkgs,
  hostname,
  stateVersion,
  ...
}:
with outputs.lib; {
  imports = [./user-macos-configs.nix];

  networking.hostName = mkDefault hostname;

  system = {
    stateVersion = mkDefault stateVersion;

    defaults = {
      SoftwareUpdate.AutomaticallyInstallMacOSUpdates = mkDefault false;
      smb.NetBIOSName = mkDefault hostname;
    };
  };

  time.timeZone = mkDefault "Europe/Oslo";

  environment.systemPackages = with pkgs; [vim git wget curl];

  services.nix-daemon.enable = mkDefault true;

  nix = {
    gc = {
      automatic = mkDefault true;
      options = mkDefault "--delete-older-than 14d";
    };
  };
}
