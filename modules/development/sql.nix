_: {
  flake.homeModules.sql =
    { pkgs, ... }:
    let
      inherit (builtins) toJSON;
    in
    {
      home.packages = with pkgs; [
        sqlit-tui
        sqlite
      ];

      # https://github.com/Maxteabag/sqlit/blob/main/config/keymap.template.json
      xdg.configFile."sqlit/keymap.json".text = toJSON {
        keymap = {
          action_keys = {
            global = {
              leader_key = ",";
            };
            autocomplete = {
              autocomplete_next = [
                "ctrl+j"
                "down"
                "tab"
              ];
              autocomplete_prev = [
                "ctrl+k"
                "up"
                "shift+tab"
              ];
              autocomplete_accept = "enter";
            };
            leader_commands = {
              delete = {
                up = [
                  "k"
                  "up"
                ];
                down = [
                  "j"
                  "down"
                ];
              };
            };
          };
        };
      };
    };
}
