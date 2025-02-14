{
  config,
  outputs,
  name,
  ...
}:
outputs.lib.mkDesktopModule config "kvm" {
  nixos = {
    programs.virt-manager.enable = true;

    users.groups.libvirtd.members = [name];

    virtualisation.libvirtd.enable = true;

    virtualisation.spiceUSBRedirection.enable = true;

    users.users."${name}".extraGroups = ["libvirtd"];
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
}
