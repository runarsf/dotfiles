_: {
  flake.nixosModules.host = {lib, ...}: {
    options.host.desktop = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Whether this host has a desktop/GUI session, as opposed to being a
        headless server. GUI-only features gate on this so they can stay in
        the shared feature list and simply no-op on headless hosts.
      '';
    };
  };
}
