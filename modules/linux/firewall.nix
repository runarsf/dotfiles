_:

{
  networking.firewall = {
    enable = false; # FIXME ooo no good ca ca
    allowedTCPPorts = [
      80
      443
    ];
  };
}
