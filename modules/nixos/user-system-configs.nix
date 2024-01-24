args@{ outputs, users, ... }:

let
  userConfigs = builtins.map (username:
    outputs.lib.mkUser {
      inherit username;
      inherit (args) system hostname stateVersion;
    }) users;

  systemConfigs = builtins.map (user: user.config.system) userConfigs;

in outputs.lib.deepMerge systemConfigs
