{
  config,
  outputs,
  pkgs,
  ...
}:
outputs.lib.mkModule config "iac" {
  home.packages = with pkgs; [
    opentofu
    terraform
  ];
}
