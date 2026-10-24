{
  self,
  withSystem,
  lib',
  ...
}:
lib'.mkUser {
  inherit self withSystem;

  root = ./.;

  username = "thomas";
  stateVersion = "24.11";

  features = [
    {
      sops = {
        privateKeys = [
          "id_priv"
          "id_ntnu"
        ];
      };
    }
    {
      ssh = {
        keys = [
          {
            name = "id_priv";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL8glmBsdfxRsQxzZrljQynBF09jljQD4KIH33Kcx9Hw";
          }
          {
            name = "id_nix";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFw8lBpuv2bWKYxxXeeG6pZ7Ut2GCtjuEbuvVEp9DmeY";
          }
          {
            name = "id_ntnu";
            key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBYghkkwi+HG+q91Xhcdc+Ac8wYdIo8BzUZKUPa2/00f";
          }
        ];
        hostKeys = {
          "git.ntnu.no" = "id_ntnu";
          "git.gvk.idi.ntnu.no" = "id_ntnu";
        };
      };
    }
    {
      stylix = {
        wallpaper = ./wallpaper.jpg;
        cursor = "wii";
      };
    }
    {
      git = {
        name = "Thomas Espervik";
        email = "thoesp@protonmail.com";
        emailSelector.extraEmails = ["thomes@stud.ntnu.no"];
      };
    }
    {
      zen = {
        customPrefs = {
          "zen.urlbar.replace-newtab" = true;
        };
      };
    }
    {
      matrix = {
        clients = [
          "element"
          "sable"
        ];
      };
    }
    "hyprland"
    "dms"
    "yazi"
    "nushell"
    "fastfetch"
    "docker"
    "japanese"
  ];

  home = {pkgs, ...}: {
    home.packages = with pkgs; [
      solidtime-desktop
    ];

    programs.zed-editor.package = pkgs.zed-editor-fhs;
  };
}
