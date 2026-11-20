import QtQuick
import Quickshell
import Quickshell.Wayland

// One screen: the image scaled to cover it, plus the moon if it is here.
PanelWindow {
    id: win

    required property var modelData
    property var moon: null // { x, y, rotation } in image pixels, or null

    screen: modelData
    anchors { top: true; bottom: true; left: true; right: true }
    exclusionMode: ExclusionMode.Ignore
    mask: Region {}
    color: "black"
    WlrLayershell.layer: WlrLayer.Background
    WlrLayershell.namespace: "quantum-moon"

    Item {
        anchors.centerIn: parent
        width: Config.imageWidth
        height: Config.imageHeight
        scale: Math.max(win.width / width, win.height / height)

        Image {
            source: "wallpaper.png"
            smooth: false
        }

        Image {
            visible: win.moon !== null
            source: "moon.png"
            smooth: false
            width: Config.moonSize
            height: Config.moonSize
            x: (win.moon?.x ?? 0) - width / 2
            y: (win.moon?.y ?? 0) - height / 2
            rotation: win.moon?.rotation ?? 0
        }
    }
}
