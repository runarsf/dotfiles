_: {
  flake.nixosModules.yubikey = {pkgs, ...}: {
    services.udev.packages = with pkgs; [yubikey-personalization];

    security.pam.u2f = {
      enable = true;
      control = "sufficient";
      settings.cue = true;
      # settings.authfile = "/etc/u2f_mappings";
    };

    security.pam.services.hyprlock.u2fAuth = true;

    security.pam.services.sshd.u2fAuth = false;
    security.pam.services.sudo.u2fAuth = true;
    security.pam.services.login.u2fAuth = true;
    security.pam.services.greetd.u2fAuth = true;
  };
}
