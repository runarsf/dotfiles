{ config, pkgs, ... }:

# https://github.com/WindSoilder/nu_plugin_bin_reader
# https://github.com/cptpiepmatz/nu-plugin-highlight
# https://github.com/FMotalleb/nu_plugin_port_list
# https://github.com/FMotalleb/nu_plugin_port_scan
# https://github.com/dead10ck/nu_plugin_dns
# https://github.com/JosephTLyons/nu_plugin_units
# https://github.com/FMotalleb/nu_plugin_image

{
  home.packages = with pkgs.nushellPlugins; [
    net
    regex
    query
    gstat
    formats
  ];

  programs.nushell = {
    enable = true;
    shellAliases = config.home.shellAliases;
    extraConfig = ''
      $env.config = {
        show_banner: false
        ls: {
          clickable_links: false
        }
        rm: {
          always_trash: true
        }
        table: {
          mode: light
        }
        completions: {
          algorithm: "fuzzy"
        }
        filesize: {
          metric: true
        }
      }
    '';
  };
}
