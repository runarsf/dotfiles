{
  config,
  pkgs,
  outputs,
  inputs,
  ...
}:
outputs.lib.mkModule config "nix" {
  # TODO nix dev alias to nix develop --command zsh
  home.sessionVariables = {
    NH_FLAKE = "${config.home.homeDirectory}/.config/nixos";
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  programs = {
    nix-index-database.comma.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
      config.global.hide_env_diff = true;
    };
  };

  home.file.".direnvrc".text =
    # bash
    ''
      # https://github.com/direnv/direnv/issues/73#issuecomment-152284914
      export_function() {
        local name=$1
        local alias_dir=$PWD/.direnv/aliases
        mkdir -p "$alias_dir"
        PATH_add "$alias_dir"
        local target="$alias_dir/$name"
        if declare -f "$name" >/dev/null; then
          echo "#!/usr/bin/env bash" > "$target"
          declare -f "$name" >> "$target" 2>/dev/null
          echo "$name" >> "$target"
          chmod +x "$target"
        fi
      }
    '';

  home = {
    packages = with pkgs; [
      alejandra
      nixd
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
    ];
  };
}
