_:

{
  services.ssh-agent.enable = true;
  programs.ssh.addKeysToAgent = "yes";
  nixos.programs.ssh.startAgent = true;

  # https://github.com/nix-community/home-manager/issues/322#issuecomment-1856128020
  home.file.".ssh/config" = {
    target = ".ssh/config_source";
    onChange = ''cat ~/.ssh/config_source > ~/.ssh/config && chmod 400 ~/.ssh/config'';
  };

  # NOTE https://github.com/nix-community/home-manager/issues/3090#issuecomment-2010891733
  #  https://github.com/nix-community/home-manager/issues/322#issuecomment-1178614454
  # nixpkgs.overlays = [
  #   (final: prev: {
  #     openssh = prev.openssh.overrideAttrs (old: {
  #       patches = (old.patches or [ ]) ++ [ ./openssh.patch ];
  #       doCheck = false;
  #     });
  #   })
  # ];
}
