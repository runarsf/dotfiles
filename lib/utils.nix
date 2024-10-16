{ outputs, ... }:

let
  sanitize = name:
    builtins.replaceStrings [ " " ] [ "-" ] (outputs.lib.toLower name);

in rec {
  # TODO Support name paths from list
  enable = elems:
    builtins.listToAttrs (map (name: {
      inherit name;
      value.enable = true;
    }) elems);

  disable = elems:
    builtins.listToAttrs (map (name: {
      name = name;
      value.enable = false;
    }) elems);

  enableIf = cond: elems: if cond then (enable elems) else (disable elems);

  print = ret: builtins.trace ret ret;

  fill = attr: value: elems:
    builtins.listToAttrs (map (name: {
      name = name;
      value."${attr}" = value;
    }) elems);
}
