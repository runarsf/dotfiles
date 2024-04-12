{
  config,
  pkgs,
  ...
}: let
  # NOTE Workaround for https://github.com/NixOS/nixpkgs/issues/197682
  inherit (config.lib.file) mkOutOfStoreSymlink;

  # Copied from `home-manager` source.
  userDir =
    if pkgs.stdenv.hostPlatform.isDarwin
    then "Library/Application Support/Code/User"
    else "${config.xdg.configHome}/Code/User";
  configFilePath = "${userDir}/settings.json";
  keybindingsFilePath = "${userDir}/keybindings.json";
in {
  programs.vscode = {
    enable = true;
    # XXX Workaround until https://github.com/nix-community/home-manager/pull/3588
    # lands in the 22.05 stable branch.
    mutableExtensionsDir = false;
    package = pkgs.unstable.vscode-with-extensions.override {
      vscodeExtensions = pkgs.unstable.callPackage ./extensions.nix {};
    } // { pname = "vscode"; };
  };

  home.file."${configFilePath}".source = ./settings.json;
  home.file."${keybindingsFilePath}".source = ./keybindings.json;
}
