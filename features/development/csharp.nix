_: {
  flake.nixosModules.csharp = _: {
    programs.nix-ld.enable = true;
  };

  flake.homeModules.csharp =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      inherit (lib) mkEnableOption optionals;

      cfg = config.features.csharp;

      dotnet =
        with pkgs.dotnetCorePackages;
        combinePackages [
          sdk_8_0
          sdk_9_0
          sdk_10_0
        ];
    in
    {
      options.features.csharp = {
        ide = mkEnableOption "C# IDE";
      };

      config = {
        home.packages =
          with pkgs;
          [
            dotnet
          ]
          ++ optionals cfg.ide [ jetbrains.rider ];

        home.sessionVariables = {
          DOTNET_ROOT = "${dotnet}/share/dotnet/";
        };
      };
    };
}
