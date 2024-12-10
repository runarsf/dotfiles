{ outputs, ... }:

let
  sanitize = name:
    builtins.replaceStrings [ " " ] [ "-" ] (outputs.lib.toLower name);

in rec {
  fill = elems: value:
    outputs.lib.foldl' (acc: elem:
      acc // outputs.lib.setAttrByPath (outputs.lib.splitString "." elem) value)
    { } elems;

  enable = elems: fill elems { enable = true; };

  disable = elems: fill elems { enable = false; };

  enableIf = cond: elems: if cond then (enable elems) else (disable elems);

  print = ret: builtins.trace ret ret;
}
