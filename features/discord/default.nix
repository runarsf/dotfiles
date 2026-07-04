_: {
  flake.homeModules.discord = {
    config,
    pkgs,
    ...
  }: let
    discordPatcher = pkgs.writers.writePython3Bin "discord-krisp-patcher" {
      libraries = with pkgs.python3Packages; [
        pyelftools
        capstone
      ];
      flakeIgnore = [
        "E265" # from nix-shell shebang
        "E501" # line too long (82 > 79 characters)
        "F403" # ‘from module import *’ used; unable to detect undefined names
        "F405" # name may be undefined, or defined from star imports: module
      ];
    } (builtins.readFile ./bin/discord-krisp-patcher.py);
  in {
    # https://github.com/NixOS/nixpkgs/issues/195512
    home.activation.krispPatch = config.lib.dag.entryAfter ["writeBoundary"] ''
      run ${pkgs.findutils}/bin/find -L ${config.home.homeDirectory}/.config/discord -name 'discord_krisp.node' -exec ${discordPatcher}/bin/discord-krisp-patcher {} \;
    '';

    home.packages = with pkgs; [
      discord
    ];
  };
}
