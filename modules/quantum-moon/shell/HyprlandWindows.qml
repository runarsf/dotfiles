import QtQuick
import Quickshell
import Quickshell.Hyprland

// Window backend for Hyprland. A backend for another compositor only has to
// expose the same `rects`: every window currently visible on any monitor, as
// { x, y, w, h } in global logical coordinates.
Scope {
    readonly property var rects: {
        // Match by name: Hyprland omits "id" for special workspaces.
        const shown = new Set();
        for (const monitor of Array.from(Hyprland.monitors.values))
            for (const ws of [monitor.lastIpcObject.activeWorkspace, monitor.lastIpcObject.specialWorkspace])
                if (ws?.name)
                    shown.add(ws.name);
        return Array.from(Hyprland.toplevels.values)
            .map(t => t.lastIpcObject)
            .filter(w => w.at && w.mapped && !w.hidden && shown.has(w.workspace?.name))
            .map(w => ({ x: w.at[0], y: w.at[1], w: w.size[0], h: w.size[1] }));
    }

    function refresh() {
        Hyprland.refreshMonitors();
        Hyprland.refreshToplevels();
    }

    Connections {
        target: Hyprland
        function onRawEvent() { refresh(); }
    }

    // Hyprland sends no event while floating windows are dragged or resized.
    Timer {
        interval: 2000
        running: true
        repeat: true
        onTriggered: refresh()
    }

    Component.onCompleted: refresh()
}
