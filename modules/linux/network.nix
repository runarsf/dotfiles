{pkgs, ...}: {
  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openconnect
      networkmanager-l2tp
    ];
  };

  programs.nm-applet.enable = true;

  # Required for Unify L2TP VPN, enable *only* MSCHAPv2 in networkmanager.
  services.strongswan = {
    enable = true;
    secrets = [
      "ipsec.d/ipsec.nm-l2tp.secrets"
    ];
  };
}
