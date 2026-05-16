{
  self,
  inputs,
  ...
}: {
  flake.nixosModules.disable-lid-switch = _: {
    services.logind = {
      lidSwitch = "ignore";
      lidSwitchDocked = "ignore";
      lidSwitchExternalPower = "ignore";
      extraConfig = "HandleLidSwitch=ignore";
    };
  };
}
