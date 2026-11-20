_: {
  flake.homeModules.claude = _: {
    programs.claude-code = {
      enable = true;
      settings = {
        includeCoAuthoredBy = false;
        defaultMode = "manual";
      };
      rules = {
        communication = ''
          # Communication
          - When a change is done, stop and tell me what to verify and test instead.
          - Be concise but complete. Lead with the answer or the change, then details.
          - Skip basic explanations; I'm experienced with Linux, DevOps, and backend/frontend dev.
          - Explain *why* for non-obvious decisions, and name tradeoffs and alternatives you considered.
          - After making changes, summarize brieflyi: what changed, where, and what I should verify.
          - If a request is ambiguous or has several reasonable approaches, ask before implementing.
          - Don't restate my request or end with generic offers of further help.
        '';
        tone = ''
          # Tone
          - Plain words only. If a word isn't in the code, a ticket, or something I said, don't use it.
          - Don't invent compound terms as shorthand. Not "route churn", "the seam", "knock-on" —
            say "the route changes", "where to hook in", "what else breaks".
          - No dramatising: "decisive", "indefensible", "worth knowing", "held in reserve".
          - Lead with the answer, and only add detail if it changes what I do.
          - Short sentences.
          '';
        workflow = ''
          # Workflow
          - Keep changes scoped to what I asked. No unrelated refactors or reformatting.
          - If you're missing documentation I could easily provide, ask before digging in the nix store.
          - Don't commit, push, or create branches. If asked to commit, don't add a Co-authored-by trailer; always commit as me.
          - Don't create README/summary/docs files unless I explicitly ask.
          - Don't create code comments unless strictly necessary.
          - Code comments must be short (shorter = better), concise, use human language, and avoid irrelevant historical data.
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
