_: let
  inherit (builtins) substring;
  run = program: "uwsm app -- ${program}";
in {
  libExtensions = [
    {
      uwsm = {
        inherit run;
        runOnce = program: "pgrep ${program} || ${run program}";
        toggle = program: let prog = substring 0 14 program; in "pkill ${prog} || ${run program}";
      };
    }
  ];
}
