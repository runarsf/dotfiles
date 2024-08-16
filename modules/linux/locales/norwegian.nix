{ lib, ... }:

# English (UK) display language, Norwegian (Bokmål) locale.

{
  time.timeZone = lib.mkDefault "Europe/Oslo";
  i18n.defaultLocale = lib.mkDefault "en_GB.UTF-8";
  console.keyMap = lib.mkDefault "no";
  services.xserver.xkb = {
    layout = lib.mkDefault "no";
    variant = lib.mkDefault "";
  };

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "nb_NO.UTF-8";
    LC_IDENTIFICATION = "nb_NO.UTF-8";
    LC_MEASUREMENT = "nb_NO.UTF-8";
    LC_MONETARY = "nb_NO.UTF-8";
    LC_NAME = "nb_NO.UTF-8";
    LC_NUMERIC = "nb_NO.UTF-8";
    LC_PAPER = "nb_NO.UTF-8";
    LC_TELEPHONE = "nb_NO.UTF-8";
    LC_TIME = "nb_NO.UTF-8";
  };
}