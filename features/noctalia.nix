{
  self,
  inputs,
  ...
}: {
  perSystem = {pkgs, ...}: {
    packages.noctalia = inputs.wrapper-modules.wrappers.noctalia-shell.wrap {
      inherit pkgs;

      settings = {
      };
    };
  };
}
