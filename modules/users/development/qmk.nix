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

  nixos = {
    hardware.keyboard.zsa.enable = true;

    services.udev = {
      packages = let
        qmk-rules = pkgs.writeTextFile {
          name = "50-qmk.rules";
          text = builtins.readFile ./qmk.rules;
          destination = "/etc/udev/rules.d/50-qmk.rules";
        };
      in [qmk-rules];
      extraRules = builtins.readFile ./qmk.rules;
    };
  };
}
