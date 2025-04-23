# This is the very core of the lib.
# IT MUST ONLY DEPEND ON ITSELF OR EXTERNAL LIBS TO AVOID INFINITE RECURSION.
{
  extlib,
  inputs,
  ...
}: rec {
  # Imports and merges all modules in a path's module's `imports` recursively.
  # Use this in case you want to resolve modules somewhere they're not, or if
  # you don't want the default merge behavior.
  resolveImports = file: args: let
    module = import file args;
  in
    if module ? imports
    then
      extlib.deepMerge ([module]
        ++ (builtins.map (submodule: resolveImports submodule args)
          module.imports))
    else module;

  # Imports and merges a list of module paths.
  importAndMerge = paths: args: let
    modules = builtins.map (file: import file args) paths;
  in
    extlib.deepMerge modules;

  # TODO This is also in nixlib
  # Override nixpkgs.lib.types.attrs to be deep-mergible. This avoids configs
  # from mistakenly overriding values due to the use of `//`.
  # types.attrs.merge = _: definitions: let
  #   values = builtins.map (definition: definition.value) definitions;
  # in
  #   extlib.deepMerge values;
}
