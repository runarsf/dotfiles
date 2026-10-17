{
  inputs,
  lib',
  ...
}:
{
  # nixosSystem with what every host shares: the `host` module, home-manager,
  # each user's nixos module, and `features` resolved by useFeatures. `imports`
  # takes non-feature modules, `configuration` the host's own settings.
  mkHost =
    {
      self,
      withSystem,
      hostName,
      stateVersion,
      users ? [ ],
      features ? [ ],
      imports ? [ ],
      configuration ? { },
      system ? "x86_64-linux",
    }:
    let
      inherit (inputs.nixpkgs.lib) genAttrs;

      hostFeatures = lib'.useFeatures self features;
    in
    withSystem system (
      { pkgs, ... }:
      inputs.nixpkgs.lib.nixosSystem {
        inherit pkgs;
        modules = [
          self.nixosModules.host
          self.nixosModules.homeManager
          {
            networking.hostName = hostName;
            system.stateVersion = stateVersion;
            home-manager.users = genAttrs users (_: {
              imports = hostFeatures.home;
            });
          }
          configuration
        ]
        ++ map (user: self.nixosModules.${user}) users
        ++ hostFeatures.nixos
        ++ imports;
      }
    );
}
