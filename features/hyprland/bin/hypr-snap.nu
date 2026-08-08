const gravity = 30

def get-geometry [client: record] -> record {
    {
        n: $client.at.1
        s: ($client.at.1 + $client.size.1)
        w: $client.at.0
        e: ($client.at.0 + $client.size.0)
    }
}

def main [] {
    let client = hyprctl -j activewindow | from json
    if not $client.floating { exit 0 }

    let workspace_id = $client.workspace.id
    let client_addr = $client.address
    let clients = hyprctl -j clients | from json
        | where { |c| $c.workspace.id == $workspace_id and $c.address != $client_addr }

    let monitor = hyprctl -j monitors | from json | where focused | first

    # getoption may be absent with non-legacy Hyprland parser; fall back to 0
    let gaps_raw = try { hyprctl -j getoption general:gaps_out | from json } catch { null }
    let border = try { hyprctl -j getoption general:border_size | from json | get int } catch { 0 }

    mut pad = { n: 0, s: 0, w: 0, e: 0 }
    if $gaps_raw != null {
        if "int" in ($gaps_raw | columns) {
            let g = $gaps_raw.int
            $pad = { n: $g, s: $g, w: $g, e: $g }
        } else {
            # custom format: "bottom top right left" (Hyprland order)
            let g = $gaps_raw.custom | split row " " | each { into int }
            $pad = {
                n: ($g | get 1)
                s: ($g | get 0)
                w: ($g | get 3)
                e: ($g | get 2)
            }
        }
    }

    $pad = {
        n: ($pad.n + $border + $monitor.reserved.1)
        s: ($pad.s + $border + $monitor.reserved.3)
        w: ($pad.w + $border + $monitor.reserved.0)
        e: ($pad.e + $border + $monitor.reserved.2)
    }

    let client_geo = get-geometry $client
    let others_geo = $clients | each { |c| get-geometry $c }

    mut closest = { n: null, s: null, w: null, e: null }

    let mon_w = $monitor.x + $pad.w
    let mon_e = $monitor.x + $monitor.width - $pad.e
    let mon_n = $monitor.y + $pad.n
    let mon_s = $monitor.y + $monitor.height - $pad.s

    if $client_geo.w < $mon_w { $closest.w = $mon_w }
    if $client_geo.e > $mon_e { $closest.e = $mon_e }
    if $client_geo.n < $mon_n { $closest.n = $mon_n }
    if $client_geo.s > $mon_s { $closest.s = $mon_s }

    for other in $others_geo {
        for edge in [n s w e] {
            let client_edge = $client_geo | get $edge
            let other_edge = $other | get $edge
            let dist = ($client_edge - $other_edge) | math abs
            if $dist <= $gravity {
                let cur = $closest | get $edge
                let cur_dist = if $cur == null {
                    $gravity + 1
                } else {
                    ($client_edge - $cur) | math abs
                }
                if $dist < $cur_dist {
                    $closest = ($closest | upsert $edge $other_edge)
                }
            }
        }
    }

    mut x = $client.at.0
    mut y = $client.at.1
    mut w = $client.size.0
    mut h = $client.size.1

    if ($closest.w != null) { $x = $closest.w }
    if ($closest.e != null) {
        if ($closest.w != null) {
            $w = ($closest.e - $closest.w)
        } else {
            $x = ($closest.e - $w)
        }
    }
    if ($closest.n != null) { $y = $closest.n }
    if ($closest.s != null) {
        if ($closest.n != null) {
            $h = ($closest.s - $closest.n)
        } else {
            $y = ($closest.s - $h)
        }
    }

    # hl.dsp.window.move_pixel / resize_pixel: verify exact param name against wiki
    hyprctl --batch $"dispatch hl.dsp.window.move_pixel\({ exact = true, x = ($x), y = ($y), address = \"($client_addr)\" }); dispatch hl.dsp.window.resize_pixel\({ exact = true, x = ($w), y = ($h), address = \"($client_addr)\" })"
}
