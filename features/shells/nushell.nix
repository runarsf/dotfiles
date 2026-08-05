{
  self,
  inputs,
  lib',
  ...
}: {
  flake.nixosModules.nushell = {pkgs, ...}: let
    inherit (pkgs.stdenv.hostPlatform) system;
  in {
    programs.nushell = {
      enable = true;
      package = self.packages.${system}.nushell;
    };
    environment.shells = [self.packages.${system}.nushell];
    environment.systemPackages = with pkgs.nushellPlugins; [
      pkgs.carapace
      query
      gstat
      formats
      # polars
    ];
  };

  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    inherit (lib) mapAttrsToList concatStringsSep;
    inherit (lib'.shell) greeting aliases;
    carapaceInit = pkgs.runCommand "carapace-init-nushell" {} ''
      ${pkgs.lib.getExe pkgs.carapace} _carapace nushell > $out
    '';
  in {
    packages.nushell = inputs.wrapper-modules.wrappers.nushell.wrap {
      inherit pkgs;

      "config.nu".content = ''
        $env.config.buffer_editor = $env.EDITOR
        $env.config.show_banner = false
        $env.config.completions.algorithm = "fuzzy"
        $env.config.use_kitty_protocol = true
        $env.config.table.index_mode = "auto"
        $env.config.table.header_on_separator = true
        $env.config.footer_mode = "auto"

        $env.CARAPACE_BRIDGES = 'zsh,bash,inshellisense'
        source ${carapaceInit}

        alias ls-builtin = ls
        def ls [
          --all (-a) = true,
          --long (-l) = true,
          --short-names (-s) = true,
          --full-paths (-f),
          --du (-d),
          --directory (-D),
          --mime-type (-m),
          --threads (-t),
          --raw (-r),
          ...pattern: glob,
        ]: [ nothing -> table ] {
          let pattern = if ($pattern | is-empty) { [ '.' ] } else { $pattern }
          (
            ls-builtin
              --all=$all
              --long=$long
              --short-names=$short_names
              --full-paths=$full_paths
              --du=$du
              --directory=$directory
              --mime-type=$mime_type
              --threads=$threads
              ...$pattern
          ) | sort-by type name -i | if $raw { table } else { move type target readonly num_links inode size created accessed modified --last | drop column 7 }
        }

        ${aliases pkgs |> mapAttrsToList (k: v: "alias ${k} = ${v}") |> concatStringsSep "\n"}

        ${greeting pkgs}
      '';
    };
  };
}
