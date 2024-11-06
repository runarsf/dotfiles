{ config, pkgs, outputs, ... }:

outputs.lib.mkModule config "nix" {
  home.sessionVariables = {
    FLAKE = "${config.home.homeDirectory}/.config/nixos";
  };

  programs.nix-index-database.comma.enable = true;

  home = {
    packages = with pkgs; [
      nil
      nixfmt
      cached-nix-shell
      deadnix
      statix
      # TODO https://github.com/viperML/nh?tab=readme-ov-file#nixos-module
      nh
      nix-inspect
      nix-output-monitor
      nvd
      nix-tree
      manix
      (pkgs.writeScriptBin "json2nix" ''
        ${pkgs.python3}/bin/python ${
          pkgs.fetchurl {
            url =
              "https://gist.githubusercontent.com/spencerpogo/0538252ed4b82d65e59115075369d34d/raw/e86d1d64d1373a497118beb1259dab149cea951d/json2nix.py";
            hash = "sha256-ROUIrOrY9Mp1F3m+bVaT+m8ASh2Bgz8VrPyyrQf9UNQ=";
          }
        } $@
      '')
    ];
  };
}
