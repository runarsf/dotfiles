_: {
  # English (UK) display language, Norwegian (Bokmål) locale.
  flake.nixosModules.norwegian = {lib, ...}: {
    time.timeZone = lib.mkDefault "Europe/Oslo";
    i18n.defaultLocale = lib.mkDefault "en_GB.UTF-8";
    console.keyMap = lib.mkDefault "no";

    services.xserver.xkb = {
      layout = lib.mkDefault "no";
      variant = lib.mkDefault "";
    };

    i18n.extraLocaleSettings = let
      locale = "nb_NO.UTF-8";
    in {
      LC_ADDRESS = locale;
      LC_IDENTIFICATION = locale;
      LC_MEASUREMENT = locale;
      LC_MONETARY = locale;
      LC_NAME = locale;
      LC_NUMERIC = locale;
      LC_PAPER = locale;
      LC_TELEPHONE = locale;
      LC_TIME = locale;
    };
  };
}
