_: {
  flake.nixosModules.orion = {lib, ...}: {
    services.flatpak = {
      remotes = lib.mkOptionDefault [
        {
          name = "orion-beta";
          location = "https://flatpak.orionbrowser.com/orion-beta.flatpakrepo";
        }
      ];
      packages = [
        {
          appId = "com.kagi.Orion";
          origin = "orion-beta";
        }
      ];
    };
  };
}
