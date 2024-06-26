args@{ outputs, users, ... }:

let
  userConfigs = builtins.map (username:
    outputs.lib.mkUser {
      inherit username;
      inherit (args) system hostname;
      stateVersion = outputs.lib.defaultStateVersion;
    }) users;

  macosConfigs = builtins.map (user: user.config.macos) userConfigs;

in outputs.lib.deepMerge macosConfigs
