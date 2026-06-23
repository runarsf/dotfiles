{lib, ...}: let
  lua = lib.generators.mkLuaInline;
  mkKeysum = mods: key:
    lib.concatStringsSep " + " (lib.filter (s: s != "") (lib.splitString " " mods) ++ [key]);
in {
  libExtensions = [
    {
      hyprland = {
        exec = cmd: "hl.dsp.exec_cmd(${builtins.toJSON cmd})";
        onStart = cmd: {_args = ["hyprland.start" (lua "function() hl.exec_cmd(${builtins.toJSON cmd}) end")];};
        kb = mods: key: dsp: flags: {
          _args = [(mkKeysum mods key) (lua dsp) flags];
        };
      };
    }
  ];
}
