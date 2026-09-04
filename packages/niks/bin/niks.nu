#!/usr/bin/env nu

# An `nh` wrapper.

def with-nix-args [body: closure] {
  with-env { NIX_CONFIG: "extra-experimental-features = nix-command flakes pipe-operators" } $body
}

def confirm [message: string = "Continue?", --default]: nothing -> bool {
  let prompt = $"($message)(ansi reset) (if $default { '(Y/n)' } else { '(y/N)' }) "
  match (input $prompt | str trim | str lowercase) {
    "y" | "yes" => true,
    "n" | "no" => false,
    "" => $default,
    _ => { print "Invalid choice."; confirm $message --default=$default }
  }
}

def after-os-switch [bin: string] {
  if (do { sudo -n true } | complete | get exit_code) != 0 { return }

  let generations = (sudo nix-env --list-generations --profile /nix/var/nix/profiles/system | lines | length)
  if $generations < 10 { return }

  if (confirm $"(ansi yellow)You have (ansi red)($generations)(ansi yellow) system generations, clean up now?(ansi reset)") {
    nh clean all --keep=3 --optimise
  } else {
    print $"  $ ($bin) clean all --keep=3 --optimise"
  }
}

def after-home-switch [bin: string] {
  let generations = (home-manager generations | lines | length)
  if $generations < 10 { return }

  if (confirm $"(ansi yellow)You have (ansi red)($generations)(ansi yellow) home generations, clean up now?(ansi reset)") {
    nh clean user --keep=3
  } else {
    print $"  $ ($bin) clean user --keep=3"
  }
}

def --wrapped main [
  --skip-substituters (-s),
  --quiet (-q),
  --extra (-e),
  --flake-dir (-d): string,
  ...args: string
] {
  let dir = if $flake_dir != null {
    $flake_dir
  } else if "NH_FLAKE" in $env {
    $env.NH_FLAKE
  } else {
    pwd
  }
  cd $dir

  let bin = "niks" # $env.CURRENT_FILE | path basename

  let separator_idx = ($args | enumerate | where item == "--" | get index? | first | default null)

  let before_sep: list<string> = if $separator_idx != null {
    $args | first $separator_idx
  } else {
    $args
  }

  let after_sep: list<string> = if $separator_idx != null {
    $args | skip ($separator_idx + 1)
  } else {
    []
  }

  let positionals = $before_sep | where { |arg| not ($arg | str starts-with "-") }
  let command    = $positionals | get 0?
  let subcommand = $positionals | get 1?

  if $command == null or $subcommand == null {
    run-external "nh" ...$args
    return
  }

  let flake_ref   = $positionals | get 2?
  let config_name = if $flake_ref != null and ($flake_ref | str contains "#") {
    $flake_ref | split row "#" | last
  } else {
    sys host | hostname
  }

  let extra_args = if $skip_substituters {
    []
  } else {
    let settings = with-nix-args {
      nix eval --json $".#nixosConfigurations.\"($config_name)\".config.nix.settings" --apply "s: { substituters = s.trusted-substituters; publicKeys = s.trusted-public-keys; }"
    } | from json

    let substituters = $settings.substituters | str join " "
    let public_keys  = $settings.publicKeys   | str join " "
    let sub_opt = if $extra { "extra-substituters" } else { "substituters" }
    let t_sub_opt = if $extra { "extra-trusted-substituters" } else { "trusted-substituters" }
    let key_opt = if $extra { "extra-trusted-public-keys" } else { "trusted-public-keys" }

    [
      "--option", $sub_opt, $substituters,
      "--option", $t_sub_opt, $substituters,
      "--option", $key_opt, $public_keys
    ]
  }

  let final_args = (
    $before_sep
    | append "--"
    | append $after_sep
    | append "--accept-flake-config"
    | append $extra_args
  )

  if not $quiet {
    let formatted = $final_args | reduce -f "" { |arg, acc|
      if ($acc | is-empty) {
        $arg
      } else if ($arg | str starts-with "-") {
        $"($acc)(char newline)    ($arg)"
      } else {
        $"($acc) ($arg)"
      }
    }
    print $"(ansi blue)info:(ansi reset) running:(char newline)$ nh ($formatted)(char newline)"
  }

  with-nix-args {
    with-env {
      NH_FLAKE: $dir
    } {
      run-external "nh" ...$final_args
    }
  }

  match [$command, $subcommand] {
    ["os", "switch"] => { after-os-switch $bin }
    ["home", "switch"] => { after-home-switch $bin }
    ["os", "test"] | ["home", "test"] => {
        print $"\n(ansi green)Build success!(ansi reset) Remember to switch:\n  $ ($bin) ($command) switch"
    }
    _ => {}
  }
}
