_: {
  flake.nixosModules.disableLidSwitch = _: {
    services.logind = {
      lidSwitch = "ignore";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "ignore";
      settings.Login.HandleLidSwitch = "ignore";
    };
  };
}
