{
  outputs,
  config,
  ...
}:
outputs.lib.mkModule config "keychain" {
  programs.keychain = {
    enable = true;
    agents = ["ssh" "gpg"];
    keys = config.modules.sops.privateKeyNames;
  };
}
