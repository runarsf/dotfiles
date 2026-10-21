_: {
  homeModule = _: {
    programs.claude-code = {
      # settings = {
      #   permissions = {
      #     additionalDirectories = [ "${config.home.homeDirectory}/Documents/notes" ];
      #   };
      # };
      rules.task-vault = ''
        # Task vault workflow (dormant unless triggered)

        Applies ONLY when the user names a task or asks to continue/resume one.
        Otherwise skip this section entirely and work normally — never mention it.

        Vault: `~/Documents/notes` — all planning, findings, and progress live there,
        never in the current repo. If that path doesn't exist, say so in one line and
        continue without it.

        ## Layout
        - `Plans/<task-name>/` containing `Task Plan.md`, `Findings.md`, `Progress.md`
        - Folder name = task name. Tasks coexist as sibling folders; never rename or merge them.

        ## Session
        - On trigger: read the named task's `Task Plan.md` + `Progress.md` first.
        - "Continue" with no task name → ask which task; don't guess.
        - Touch only the named task's folder. Other task folders are off-limits even for reference unless asked.
        - No folder for the named task yet → confirm whether to scaffold the three files or wait for the user to populate them.
        - Work only the current phase unless redirected. Never write plan/progress/findings/notes files in the repo.

        ## Mid-task questions
        - Batch non-blocking questions instead of asking one at a time.
        - Minor, easily reversible: decide, act, log decision + why in `Findings.md`.
        - Irreversible or scope-changing: stop and ask.
        - Working unattended: append questions with a proposed default under `Open questions` in the task's `Task Plan.md`; proceed only if reversible.

        ## Session end — always
        Append dated (ISO) entries; never overwrite or reorder existing content:
        - `Findings.md` — decisions + why, gotchas, dead ends, key files touched
        - `Progress.md` — what was done, what was verified, exact next step
        - `Task Plan.md` — tick completed phases; adjust later phases if reality diverged

        ## Vault etiquette
        - Ask before deleting or renaming notes; never touch any folder outside of `**/agent` or attachments
        - Keep YAML frontmatter intact; use `[[wikilinks]]`
        - No secrets in vault notes
      '';
    };
  };
}
