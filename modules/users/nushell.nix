{
  config,
  pkgs,
  outputs,
  ...
}:
# https://github.com/WindSoilder/nu_plugin_bin_reader
# https://github.com/cptpiepmatz/nu-plugin-highlight
# https://github.com/FMotalleb/nu_plugin_port_list
# https://github.com/FMotalleb/nu_plugin_port_scan
# https://github.com/dead10ck/nu_plugin_dns
# https://github.com/JosephTLyons/nu_plugin_units
# https://github.com/FMotalleb/nu_plugin_image
outputs.lib.mkModule config "nushell" {
  home.packages = with pkgs.nushellPlugins; [
    net
    query
    gstat
    formats
  ];

  programs.nushell = {
    enable = true;
    environmentVariables = {
      HELLO = "WORLD";
    };
    # extraConfig = ''
    #   $env.config = {
    #     show_banner: false
    #     ls: {
    #       clickable_links: false
    #     }
    #     rm: {
    #       always_trash: true
    #     }
    #     table: {
    #       mode: light
    #     }
    #     completions: {
    #       algorithm: "fuzzy"
    #     }
    #     filesize: {
    #       metric: true
    #     }
    #   }
    # '';
  };
}
