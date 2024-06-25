_:

{
  enable = attrs: builtins.listToAttrs (map (name: {
    name = name;
    value.enable = true;
  }) attrs);

  disable = attrs: builtins.listToAttrs (map (name: {
    name = name;
    value.enable = false;
  }) attrs);
}
