{ self, lib', ... }:
{
  flake.homeModules.migrations = lib'.mkModule "migrations" (
    {
      config,
      lib,
      pkgs,
      ...
    }:
    let
      inherit (lib)
        mkOption
        types
        mapAttrs'
        mapAttrsToList
        nameValuePair
        filterAttrs
        optionalString
        escapeShellArgs
        ;
      inherit (pkgs.stdenv.hostPlatform) system;

      cfg = config.modules.migrations;
      hm-migrate = self.packages.${system}.hm-migrate;
      markers = "${config.xdg.stateHome}/home-manager/migrations";

      entries = filterAttrs (_: entry: entry.enable) cfg.entries;

      # One activation entry per step: a step that has to beat another module's
      # activation to the files it writes needs its own place in the dag.
      step =
        name: entry:
        ''
          migrate_run=1
        ''
        + optionalString entry.once ''
          if [ -e "${markers}/${name}" ]; then
            migrate_run=0
          fi
        ''
        + optionalString (entry.when != null) ''
          if [ "$migrate_run" = 1 ] && ! ( ${entry.when} ); then
            migrate_run=0
          fi
        ''
        + optionalString (entry.locks != [ ]) ''
          if [ "$migrate_run" = 1 ]; then
            migrate_busy=$(${hm-migrate}/bin/hm-migrate busy ${escapeShellArgs entry.locks})
            if [ -n "$migrate_busy" ]; then
              echo "migrate ${name}: skipped, $migrate_busy is still open"
              migrate_run=0
            fi
          fi
        ''
        + ''
          if [ "$migrate_run" = 1 ]; then
            (
              PATH="${hm-migrate}/bin:$PATH"
              ${entry.script}
            )
        ''
        + optionalString entry.once ''
            run mkdir -p "${markers}"
            run touch "${markers}/${name}"
        ''
        + ''
          fi
        '';

      entryModule = {
        options = {
          enable = mkOption {
            type = types.bool;
            default = true;
            description = "Whether to run this step.";
          };
          when = mkOption {
            type = types.nullOr types.lines;
            default = null;
            description = ''
              Shell test deciding whether there is anything to do. A step whose
              work makes its own test false needs no other bookkeeping.
            '';
          };
          once = mkOption {
            type = types.bool;
            default = false;
            description = ''
              Record the step under XDG_STATE_HOME and never run it again. For
              steps with nothing to test; delete the marker to re-run.
            '';
          };
          locks = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = ''
              Absolute glob patterns. While anything holds one of them open the
              step is left for the next switch. Not shell-expanded, so no `$HOME`.
            '';
          };
          before = mkOption {
            type = types.listOf types.str;
            default = [ ];
            description = "Activation entries that must run after this step.";
          };
          after = mkOption {
            type = types.listOf types.str;
            default = [ "writeBoundary" ];
            description = "Activation entries this step must run after.";
          };
          script = mkOption {
            type = types.lines;
            description = ''
              The step itself. `hm-migrate` is on PATH, and `run` makes a
              command obey --dry-run.
            '';
          };
        };
      };
    in
    {
      options.modules.migrations.entries = mkOption {
        type = types.attrsOf (types.submodule entryModule);
        default = { };
        description = "One-shot steps that bring existing state up to what the config now expects.";
      };

      config = {
        assertions = mapAttrsToList (name: entry: {
          assertion = entry.when != null || entry.once;
          message = "modules.migrations.entries.${name} needs 'when' or 'once', or it runs on every switch.";
        }) entries;

        home.activation = mapAttrs' (
          name: entry:
          nameValuePair "migrate-${name}" (
            lib.hm.dag.entryBetween entry.before entry.after (step name entry)
          )
        ) entries;
      };
    }
  );
}
