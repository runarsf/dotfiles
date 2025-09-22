{
  config,
  pkgs,
  outputs,
  ...
}:
outputs.lib.mkModule config "qmk" {
  home.packages = with pkgs; [
    qmk
    keymapp
  ];

  nixos.hardware.keyboard.zsa.enable = true;
  modules.udev.extraRules = [ ./qmk.rules ];
}
