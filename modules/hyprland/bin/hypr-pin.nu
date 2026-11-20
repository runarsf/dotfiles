def main [] {
    let client = hyprctl -j activewindow | from json
    let dat = $"/tmp/hypr-($client.pid).dat"

    if not $client.floating {
        hyprctl dispatch "hl.dsp.window.float({ action = \"toggle\" })"
    }

    hyprctl dispatch "hl.dsp.window.pin()"

    if $client.pinned {
        notify-send "Hypr" "Unpinned window"
        let saved = open $dat | from json
        if not $saved.floating {
            hyprctl dispatch "hl.dsp.window.float({ action = \"toggle\" })"
        }
        rm --force $dat
    } else {
        notify-send "Hypr" "Pinned window"
        $client | to json | save --force $dat
    }
}
