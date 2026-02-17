_: let
  inherit (builtins) substring;
in rec {
  run = program: "uwsm app -- ${program}";

  toggle = program: let
    prog = substring 0 14 program;
  in "pkill ${prog} || ${run program}";

  runOnce = program: "pgrep ${program} || ${run program}";
}
