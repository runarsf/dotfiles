_: {
  flake.homeModules.claude = _: {
    programs.claude-code = {
      enable = true;
      settings = {
        includeCoAuthoredBy = false;
        defaultMode = "manual";
      };
      rules = {
        testing = ''
          # Testing
          - Never run tests, test suites, or `nix flake check`. I test myself.
          - Don't write or modify test files unless I explicitly ask.
          - When a change is done, stop and tell me what to verify instead.
        '';
        communication = ''
          # Communication
          - Be concise but complete. Lead with the answer or the change, then details.
          - Skip basic explanations; I'm experienced with Linux, DevOps, and backend/frontend dev.
          - Explain *why* for non-obvious decisions, and name tradeoffs and alternatives you considered.
          - After making changes, summarize brieflyi: what changed, where, and what I should verify.
          - If a request is ambiguous or has several reasonable approaches, ask before implementing.
          - Don't restate my request or end with generic offers of further help.
        '';
        workflow = ''
          # Workflow
          - Keep changes scoped to what I asked. No unrelated refactors or reformatting.
          - If you're missing documentation I could easily provide, ask before digging in the nix store.
          - Don't commit, push, or create branches. If asked to commit, don't add a Co-authored-by trailer; always commit as me.
          - Don't create README/summary/docs files unless I ask.
          - Don't create code comments unless strictly necessary.
          - Code comments must be short and concise, and avoid irrelevant historical data.
          - Never format code with commands; I prefer formatting myself.
        '';
        environment = ''
          # Environment
          - This is NixOS with a flake-based setup. Never install tools globally (pip install, npm -g, curl | sh).
            Use `nix shell nixpkgs#<pkg>` or the project's `nix develop` shell if present.
          - Don't run `nix flake update` or touch flake.lock unless I ask.
          - Files in ~/.claude are managed by Home Manager and read-only; don't try to edit them.
          - I don't have python, prefer other tools (jq, grep, ...).
          - Wayland desktop. Don't assume X11 tools like xclip or xdotool; use wl-clipboard etc.
        '';
      };
    };
  };
}
