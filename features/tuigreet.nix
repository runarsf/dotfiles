{ ... }: {
  flake.nixosModules.tuigreet =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      # Filter out uwsm-managed session entries (e.g. "Hyprland (uwsm-managed)")
      # that packages like Hyprland ship unconditionally, regardless of whether
      # uwsm is actually configured/installed.
      filteredSessions = pkgs.runCommand "wayland-sessions-no-uwsm" { } ''
        mkdir -p "$out"
        for f in "${config.services.displayManager.sessionData.desktops}"/share/wayland-sessions/*.desktop; do
          if ! grep -qi 'uwsm' "$f"; then
            cp "$f" "$out"/
          fi
        done
      '';
    in
    {
      services.greetd = {
        enable = true;
        settings.default_session = {
          user = "greeter";
          command = lib.concatStringsSep " " [
            (lib.getExe pkgs.tuigreet)
            "--time --remember --remember-session"
            "--sessions ${filteredSessions}"
            "--theme 'container=black;border=darkgray;text=gray;time=darkgray;title=darkgray;greet=gray;prompt=gray;input=white;action=darkgray;button=darkgray'"
          ];
        };
      };
    };
}
