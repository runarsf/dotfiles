{
  config,
  pkgs,
  outputs,
  ...
}:

# TODO https://www.nushell.sh/cookbook/external_completers.html#zoxide-completer
# TODO Remove empty columns from ls by default

outputs.lib.mkModule config "nushell" {
  # https://github.com/yybit/nu_plugin_compress
  # https://github.com/dead10ck/nu_plugin_dns
  # https://github.com/b4nst/nu_plugin_format_pcap
  # https://crates.io/crates/nu_plugin_hashes
  # https://github.com/FMotalleb/nu_plugin_port_extension
  # https://github.com/fdncred/nu_plugin_regex
  home.packages = with pkgs;
  with nushellPlugins; [
    # net
    query
    gstat
    formats
    polars
    # highlight

    carapace
    carapace-bridge
  ];

  programs.nushell = let
    carapaceCache = "${config.home.homeDirectory}/.cache/carapace";
  in {
    environmentVariables = config.home.sessionVariables;
    enable = true;
    # config nu --doc | nu-highlight | less -R
    configFile.text = ''
      $env.config.buffer_editor = $env.EDITOR
      $env.config.show_banner = false
      $env.config.completions.algorithm = "fuzzy"
      $env.config.use_kitty_protocol = true
      $env.config.table.index_mode = "auto"
      $env.config.table.header_on_separator = true
      $env.config.footer_mode = "auto"

      $env.CARAPACE_BRIDGES = 'zsh,bash,inshellisense'
      source ${carapaceCache}/init.nu

      alias ls-builtin = ls
      def ls [
        --all (-a) = true,         # Show hidden files
        --long (-l) = true,        # Get all available columns for each entry (slower; columns are platform-dependent)
        --short-names (-s) = true, # Only print the file names, and not the path
        --full-paths (-f),         # display paths as absolute paths
        --du (-d),                 # Display the apparent directory size ("disk usage") in place of the directory metadata size
        --directory (-D),          # List the specified directory itself instead of its contents
        --mime-type (-m),          # Show mime-type in type column instead of 'file' (based on filenames only; files' contents are not examined)
        --threads (-t),            # Use multiple threads to list contents. Output will be non-deterministic.
        --raw (-r),                # Disable output filtering
        ...pattern: glob,          # The glob pattern to use.
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
    '';
    extraConfig = ''
      ${outputs.lib.getExe pkgs.krabby} random --no-title
    '';
    envFile.text = ''
      let carapace_cache = "${carapaceCache}"
      if not ($carapace_cache | path exists) {
        mkdir $carapace_cache
      }
      ${outputs.lib.getExe pkgs.carapace} _carapace nushell | save --force $"($carapace_cache)/init.nu"
    '';
  };
}
