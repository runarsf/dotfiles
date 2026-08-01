#!/usr/bin/env nu

# Workspace reference (name, index, or id) games should go to.
const GAMING_WS = "gaming"

def proc-comm [pid: int]: nothing -> string {
    try { ^cat $"/proc/($pid)/comm" | str trim } catch { "" }
}

def proc-ppid [pid: int]: nothing -> int {
    try {
        ^cat $"/proc/($pid)/status"
        | lines
        | where { str starts-with "PPid:" }
        | first
        | str replace "PPid:" ""
        | str trim
        | into int
    } catch { 0 }
}

# Search /proc for a live process whose comm matches name. Used as fallback
# when _NET_WM_PID is not set by the X11 client (e.g. Unity/SDL games).
def find-pid-by-comm [name: string]: nothing -> int {
    let prefix = ($name | str substring 0..14)
    try {
        ls /proc
        | get name
        | path basename
        | where { $in =~ '^\d+$' }
        | each { into int }
        | where { |p| (proc-comm $p) == $prefix }
        | first
    } catch { 0 }
}

# For XWayland windows niri reports xwayland-satellite's pid rather than the
# game's. Detect this and resolve the real pid: try xdotool (_NET_WM_PID)
# first; fall back to a /proc comm scan for apps that don't set _NET_WM_PID
# (e.g. Unity games like Valheim).
def resolve-pid [pid: int, app_id: string]: nothing -> int {
    if not ((proc-comm $pid) | str contains -i "xwayland") { return $pid }

    let via_x11 = (try {
        let xid = (^xdotool search --class $app_id
            | lines
            | where { not ($in | is-empty) }
            | first)
        ^xdotool getwindowpid $xid | str trim | into int
    } catch { 0 })
    if $via_x11 > 1 { return $via_x11 }

    find-pid-by-comm $app_id
}

# Steam always launches games via its reaper child-monitor with SteamLaunch as
# the first argument. Walking the tree to find this is world-readable (unlike
# /proc/<pid>/environ) and avoids false-positives from the Steam UI itself,
# which is NOT launched through reaper.
def is-steam-game [pid: int]: nothing -> bool {
    mut current = $pid
    loop {
        if (proc-comm $current) == "reaper" {
            let cmdline = (try { ^cat $"/proc/($current)/cmdline" | decode utf-8 } catch { "" })
            if ($cmdline | str contains "SteamLaunch") { return true }
        }
        let ppid = (proc-ppid $current)
        if $ppid <= 1 { break }
        $current = $ppid
    }
    false
}

def vrr-set [gaming: bool] {
    try {
        let outputs = (niri msg --json outputs | from json | values)
        for o in ($outputs | where vrr_supported) {
            if $gaming {
                niri msg output $o.name vrr on
            } else {
                niri msg output $o.name vrr on --on-demand
            }
        }
    }
}

def main [--verbose(-v)] {
    mut seen  = []  # all evaluated window IDs (prevents re-processing on focus/title events)
    mut games = []  # IDs of confirmed steam game windows (drives VRR state)

    # `for` (not `each`) so we can mutate `seen` and `games`; it still streams lazily.
    for line in (niri msg --json event-stream | lines) {
        let event = ($line | from json)

        let closed = ($event.WindowClosed?.id?)
        if $closed != null {
            $seen = ($seen | where {|x| $x != $closed})
            if $closed in $games {
                $games = ($games | where {|x| $x != $closed})
                if $verbose { print $"[closed] steam game window ($closed) — ($games | length) remaining" }
                if ($games | is-empty) {
                    if $verbose { print "[vrr] no games running — reverting to on-demand" }
                    vrr-set false
                }
            } else {
                if $verbose { print $"[closed] window ($closed)" }
            }
            continue
        }

        let win = ($event.WindowOpenedOrChanged?.window?)
        if $win == null { continue }

        let id     = $win.id
        let app_id = $win.app_id

        # Skip windows already evaluated — WindowOpenedOrChanged fires on focus
        # changes and title updates too, not just creation.
        if ($id in $seen) { continue }
        $seen = ($seen | append $id)

        let pid = $win.pid
        if $pid == null {
            if $verbose { print $"[skip] window ($id) app=($app_id) — pid unresolved" }
            continue
        }

        let real_pid = (resolve-pid $pid $app_id)
        if $real_pid <= 0 {
            if $verbose { print $"[skip] window ($id) app=($app_id) pid=($pid) — could not resolve real pid" }
            continue
        }

        if (is-steam-game $real_pid) {
            if $verbose { print $"[steam] window ($id) app=($app_id) pid=($real_pid) — moving to ($GAMING_WS) and fullscreening" }
            $games = ($games | append $id)
            if ($games | length) == 1 {
                if $verbose { print "[vrr] first game opened — enabling VRR" }
                vrr-set true
            }
            niri msg action move-window-to-workspace $GAMING_WS --window-id $id
        } else {
            if $verbose { print $"[miss] window ($id) app=($app_id) pid=($real_pid) — not a steam game" }
        }
    }
}
