{lib, ...}: let
  inherit (builtins) toJSON;
  inherit (lib) filter splitString concatStringsSep;

  lua = lib.generators.mkLuaInline;
  mkKeysum = mods: key:
    concatStringsSep " + " (filter (s: s != "") (splitString " " mods) ++ [key]);
in {
  libExtensions = [
    {
      hyprland = {
        inherit lua mkKeysum;

        exec = cmd: "hl.dsp.exec_cmd(${toJSON cmd})";
        onStart = cmd: {_args = ["hyprland.start" (lua "function() hl.exec_cmd(${toJSON cmd}) end")];};
        kb = mods: key: dsp: flags: {
          _args = [(mkKeysum mods key) (lua dsp) flags];
        };
      };
    }
  ];
}
