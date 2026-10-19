{ lib', ... }:
let
  inherit (builtins) readFile attrNames;
  inherit (lib'.jsonc) fromJSONCPure;

  bitmap = true;

  userSettings = ./settings.json |> readFile |> fromJSONCPure;
in
{
  flake.homeModules.zed-runar = _: {
    programs.zed-editor = {
      userSettings = (removeAttrs userSettings [ "auto_install_extensions" ]) // {
        buffer_font_family = if bitmap then "ScientificaVector" else "CaskaydiaCove Nerd Font";
        buffer_font_size = if bitmap then 22 else 18;
      };

      userKeymaps = ./keymap.json |> readFile |> fromJSONCPure;

      userTasks = ./tasks.json |> readFile |> fromJSONCPure;

      # jq '{auto_install_extensions: (.extensions | map({key: .manifest.id, value: true}) | from_entries)}' ~/.local/share/zed/extensions/index.json
      extensions = attrNames userSettings.auto_install_extensions;
    };
  };
}
