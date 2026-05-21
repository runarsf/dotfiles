#!/usr/bin/env nu

# An `nh` wrapper.

def confirm [message: string = "Continue?", --default]: nothing -> bool {
  let prompt = $"($message)(ansi reset) (if $default { '(Y/n)' } else { '(y/N)' }) "
  match (input $prompt | str trim | str downcase) {
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
    nh clean all --keep=3
  } else {
    print $"  $ ($bin) clean all --keep=3"
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

def --wrapped main [...args: string] {
  if "NH_FLAKE" in $env {
    cd $env.NH_FLAKE
  }

  if ($args | any { |a| $a | str starts-with "-" }) {
    run-external "nh" ...$args
    return
  }

  let bin = ($env.CURRENT_FILE | path basename)
  let hostname = sys host | hostname

  let command    = ($args | get 0)
  let subcommand = ($args | get 1)
  let rest       = ($args | skip 2)
  let separator_idx = ($rest | enumerate | where item == "--" | get index? | first | default null)

  let before_sep: list<string> = if $separator_idx != null {
    $rest | first $separator_idx
  } else {
    $rest
  }

  let after_sep: list<string> = if $separator_idx != null {
    $rest | skip ($separator_idx + 1)
  } else {
    []
  }

  let substituters = (
    nix eval --json $".#nixosConfigurations.\"($hostname)\".config.nix.settings.trusted-substituters"
    | from json
    | str join " "
  )

  let public_keys = (
    nix eval --json $".#nixosConfigurations.\"($hostname)\".config.nix.settings.trusted-public-keys"
    | from json
    | str join " "
  )

  # Build the final argument list:
  #   <command> <subcommand> <before-sep> -- <after-sep> --accept-flake-config
  let final_args = (
    [$command, $subcommand]
    | append $before_sep
    | append "--"
    | append $after_sep
    | append "--accept-flake-config"
    | append ["--option", "substituters", $"($substituters)"]
    | append ["--option", "trusted-public-keys", $"($public_keys)"]
  )

  run-external "nh" ...$final_args

  match [$command, $subcommand] {
    ["os", "switch"] => { after-os-switch $bin }
    ["home", "switch"] => { after-home-switch $bin }
    ["os", "test"] | ["home", "test"] => {
        print $"\n(ansi green)Build success!(ansi reset) Remember to switch:\n  $ ($bin) ($command) switch"
    }
    _ => {}
  }
}
