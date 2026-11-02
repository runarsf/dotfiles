_: {
  homeModule = {config, ...}: {
    programs.claude-code = {
      settings = {
        permissions = {
          additionalDirectories = [ "${config.home.homeDirectory}/Documents/notes/Work" ];
        };
      };
      rules.task-vault = ''
        # Task vault workflow

        This section only applies when working/asked to work on a task that exists in `~/Documents/notes/Work/Tasks/`.
        Otherwise skip this section entirely and work normally — never mention it.

        Vault: `~/Documents/notes/Work` — all planning, findings, and progress live there,
        never in the current repo.

        ## Layout
        - `Tasks/<task-name>/` containing `Plan.md`, `Findings.md`, `Progress.md`, among other related documentation.
        - Tasks coexist as sibling folders; never rename or merge them.
        - Always be short and concise when documenting, and use human language rather than overcomplicated words.
        - `Tickets/<ticket-name>` containing git issues; not scoped to any plans. Never edit tickets, but use them for reference.

        ## Session
        - On trigger: read the named task's `Task Plan.md` + `Progress.md` first.
        - Modify only the named task's folder. Other task folders are off-limits, except for reference.
        - No folder for the named task yet → confirm whether to scaffold the three files or wait for the user to populate them.
        - Work only the current phase unless redirected. Never write plan/progress/findings/notes files in the repo.
        - Always stop and ask when decisions/questions arise, but consult tickets and provide options.

        ## Session end — always
        Append dated (ISO) entries; never overwrite or reorder existing content:
        - `Findings.md` — decisions + why, gotchas, dead ends, key files touched
        - `Progress.md` — what was done, what was verified, exact next step
        - `Task Plan.md` — tick completed phases; adjust later phases if reality diverged

        ## Vault etiquette
        - Ask before deleting or renaming notes.
        - Never touch YAML frontmatter unless prompted, keep it intact, but never update; use `[[wikilinks]]` when referring to other files.
        - Never store secrets in vault notes.
      '';
    };
  };
}
