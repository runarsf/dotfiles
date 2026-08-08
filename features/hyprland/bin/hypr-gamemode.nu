def flag [] { $"($env.XDG_RUNTIME_DIR)/hypr-gamemode-active" }

def has-effects [] -> bool {
    not ((flag) | path exists)
}

def disable-effects [] {
    touch (flag)
    hyprctl eval "
        hl.config.general.gaps_in = 0
        hl.config.general.gaps_out = 0
        hl.config.general.border_size = 1
        hl.config.general.allow_tearing = true
        hl.config.decoration.rounding = 0
        hl.config.decoration.blur.enabled = false
        hl.config.decoration.shadow.enabled = false
        hl.config.animations.enabled = false
        hl.config.plugin[\"dynamic-cursors\"].enabled = false
    "
}

def restore-effects [] {
    rm --force (flag)
    hyprctl reload
}

def detect-gamemode [] {
    let ws10 = hyprctl -j workspaces | from json | where name == "10"
    if ($ws10 | is-not-empty) and (($ws10 | first | get windows) > 0) {
        disable-effects
    } else if not (has-effects) {
        restore-effects
    }
}

def main [action?: string] {
    match $action {
        "toggle" => {
            if (has-effects) { disable-effects } else { restore-effects }
        },
        _ => {
            rm --force (flag)
            let socket = $"($env.XDG_RUNTIME_DIR)/hypr/($env.HYPRLAND_INSTANCE_SIGNATURE)/.socket2.sock"
            nc -U $socket | lines | each { |line|
                if ($line | split row ">>" | first) in ["openwindow" "closewindow"] {
                    detect-gamemode
                }
            } | ignore
        }
    }
}
