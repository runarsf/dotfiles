{ config, lib, pkgs, inputs, ... }:
let
  domain = "runar.ch";
  email = "i@${domain}";
  cert = "/var/lib/acme/${domain}/cert.pem";
  key = "/var/lib/acme/${domain}/key.pem";
in {
  # https://github.com/lucernae/nixos-pi
  imports = [
    inputs.nixos-hardware.nixosModules.raspberry-pi-4

    # nixpkgs/nixos/modules/installer/sd-card/sd-image-aarch64-installer.nix
    # nixpkgs/nixos/modules/installer/cd-dvd/channel.nix
    ../common/podman.nix
    (import ../common/nginx.nix { inherit config domain cert key email pkgs; })
    (import ../common/containers/pialert.nix { inherit config domain cert key email; })
  ];

  system.stateVersion = "23.11";

  # sdImage.compressImage = false;

  # NixOS wants to enable GRUB by default
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;

  # !!! Set to specific linux kernel version
  boot.kernelPackages = pkgs.linuxPackages_6_1;

  # !!! Needed for the virtual console to work on the RPi 3, as the default of 16M doesn't seem to be enough.
  # If X.org behaves weirdly (I only saw the cursor) then try increasing this to 256M.
  # On a Raspberry Pi 4 with 4 GB, you should either disable this parameter or increase to at least 64M if you want the USB ports to work.
  boot.kernelParams = [ "cma=256M" ];

  # Settings above are the bare minimum
  # All settings below are customized depending on your needs

  nixpkgs.overlays =
    [ (final: super: { makeModulesClosure = x: super.makeModulesClosure (x // { allowMissing = true; }); }) ];

  # systemPackages
  environment.systemPackages = with pkgs; [ vim curl wget nano bind iptables openvpn python3 nodejs_20 docker ];

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  programs.zsh = {
    enable = true;
    ohMyZsh = {
      enable = true;
      theme = "miloshadzic";
    };
  };

  virtualisation.docker.enable = true;

  # WiFi
  hardware = {
    enableRedistributableFirmware = true;
    firmware = [ pkgs.wireless-regdb ];
  };

  # Networking
  networking = {
    # useDHCP = true;
    interfaces.wlan0 = {
      useDHCP = true;
      # ipv4.addresses = [{
      #   # I used static IP over WLAN because I want to use it as local DNS resolver
      #   address = "192.168.100.4";
      #   prefixLength = 24;
      # }];
    };
    interfaces.eth0 = {
      useDHCP = true;
      # I used DHCP because sometimes I disconnect the LAN cable
      #ipv4.addresses = [{
      #  address = "192.168.100.3";
      #  prefixLength = 24;
      #}];
    };

    # Enabling WIFI
    wireless.enable = true;
    wireless.interfaces = [ "wlan0" ];
    # If you want to connect also via WIFI to your router
    wireless.networks."p7p".psk = "bingchillingsss";
    wireless.networks."Bern".psk = "gagatondra";
    # You can set default nameservers
    # nameservers = [ "1.1.1.1", "1.0.0.1" ];
    # You can set default gateway
    # defaultGateway = {
    #   address = "192.168.100.1";
    #   interface = "wlan0";
    # };
  };

  # put your own configuration here, for example ssh keys:
  users.defaultUserShell = pkgs.zsh;
  users.mutableUsers = true;
  users.groups = {
    nixos = {
      gid = 1000;
      name = "nixos";
    };
  };
  users.users = {
    nixos = {
      isNormalUser = true;
      uid = 1000;
      home = "/home/nixos";
      name = "nixos";
      group = "nixos";
      shell = pkgs.zsh;
      extraGroups = [ "wheel" "docker" ];
    };
  };
  users.extraUsers.root.openssh.authorizedKeys.keys =
    [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO8PMmq0UL+Q9SRoKV6iWD6NNlINSir5HGdQh/tL3Pre runarsf" ];

  networking = {
    nat = {
      enable = true;
      externalInterface = "eth0";
      internalInterfaces = [ "wg0" ];
    };

    firewall = {
      enable = true;
      allowedUDPPorts = [ 51820 ];
    };

    wireguard.interfaces = {
      wg0 = rec {
        ips = [ "10.100.0.1/24" ];
        listenPort = 51820;
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        privateKeyFile = "/root/.config/wireguard/private";
        peers = [{ # edu
          publicKey = "tTmKN+/IsrrWxsdhttjvpYVVAmLkb7oyed/o8FCPvRo=";
          allowedIPs = [ "10.100.0.2/32" ];
        }];
      };
    };
  };
}

