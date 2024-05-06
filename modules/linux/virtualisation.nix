{ pkgs, ... }:

{
  # TODO Add user to libvirtd group
  # FIXME This makes the rebuild really slow (because it needs to restart the virtualisation services)
  virtualisation = {
    libvirtd.enable = true;
    virtualbox.host.enable = true;
    virtualbox.guest.enable = true;
  };
  environment.systemPackages = with pkgs; [ vagrant ];
  boot.kernelModules = [ "kvm-amd" "kvm-intel" ];
}
