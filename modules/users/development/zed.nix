{
  outputs,
  config,
  pkgs,
  ...
}:
outputs.lib.mkDesktopModule config "zed" {
  programs = {
    zed-editor = {
      enable = true;
      package = pkgs.master.zed-editor;
    };
    television.enable = true;
  };

  home.packages = with pkgs; [
    ansifilter
  ];
}
