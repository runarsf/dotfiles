{ pkgs, ... }:

{
  system = {
    services.pcscd.enable = true;
    programs.gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
      # pinentryPackage = pkgs.pinentry-curses;
      # enableSSHSupport = if osConfig.services.openssh.enable
      # && !osConfig.programs.ssh.startAgent then
      #   false
      # else
      #   true;
    };
    environment.systemPackages = with pkgs; [ pinentry pinentry-curses ];
  };
}
