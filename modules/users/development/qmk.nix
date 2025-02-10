{
  config,
  pkgs,
  outputs,
  ...
}:

outputs.lib.mkModule config "qmk" {
  home.packages = with pkgs; [
    qmk
  ];

  nixos.services.udev.packages =
    let
      qmk-rules = pkgs.writeTextFile {
        name = "50-qmk.rules";
        text = builtins.readFile ./qmk.rules;
        destination = "/etc/udev/rules.d/50-qmk.rules";
      };
    in
    [ qmk-rules ];
  nixos.services.udev.extraRules = builtins.readFile ./qmk.rules;
}
