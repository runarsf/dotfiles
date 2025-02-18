{ outputs, ... }:

rec {
  fill = value: elems:
    outputs.lib.foldl' (acc: elem:
      acc // outputs.lib.setAttrByPath (outputs.lib.splitString "." elem) value)
    { } elems;

  enable = elems: fill { enable = true; } elems;

  disable = elems: fill { enable = false; } elems;

  enableIf = cond: elems: if cond then (enable elems) else (disable elems);

  print = ret: builtins.trace ret ret;

  anything = outputs.lib.types.mkOptionType {
    name = "anything";
    description = "Anything!";
    check = val: true;
  };

  # TODO mkDesktopOption 

  # TODO If it starts with slash, die. Should also handle /home/username
  # TODO Should support both xdg.configFile and home.file
  # TODO Refactor to use this function
  mkEditableHomeFile = pkgs: target: text:
    let
      pathlist = target
        |> outputs.lib.splitString "/";
      filename = pathlist
        |> outputs.lib.last;
      filepath = pathlist
        |> outputs.lib.take (outputs.lib.length pathlist - 1);
      file = filepath ++ [ filename ]
        |> builtins.concatStringsSep "/";
      sourceFile = filepath ++ [ ("." + filename) ]
        |> builtins.concatStringsSep "/";
      relativePath = file: file
        |> builtins.replaceStrings [ "~/" ] [ "" ];
      absolutePath = file: file
        |> (s: "~/" + s)
        |> builtins.replaceStrings [ "~/~/" ] [ "~/" ];
    in {
      "${relativePath file}" = {
        inherit text;
        target = relativePath sourceFile;
        onChange = if outputs.lib.hasSuffix ".json" filename then
          "${pkgs.jq}/bin/jq -s '.[0] * (.[1] | {})' ${absolutePath file} ${absolutePath sourceFile} | ${pkgs.moreutils}/bin/sponge ${absolutePath file}"
        else
          "cat ${absolutePath sourceFile} > ${absolutePath file}";
      };
    };

}
