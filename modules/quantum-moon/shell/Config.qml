pragma Singleton
import Quickshell

// All coordinates are in wallpaper.png pixels.
Singleton {
    readonly property int imageWidth: 2560
    readonly property int imageHeight: 1440

    readonly property var sun: ({ x: 315, y: 315 })

    // Where moon.png was cut from. Its lit side faces the sun from here.
    readonly property var moonOrigin: ({ x: 1742, y: 1155 })
    readonly property int moonSize: 68 // width of moon.png

    // ms to wait after windows change before checking. Hyprland reports a
    // window's final geometry as soon as it starts animating, so set this to
    // your longest window/workspace animation.
    readonly property int delay: 500

    readonly property real eyeChance: 0.03
    readonly property int samples: 24 // points tried per ring when looking for a hidden spot

    // Each location is a ring around a center; the moon sits somewhere on it.
    // `skip` lists blocked [from, to] arcs in degrees: 0 = right, clockwise.
    readonly property var planets: ({
        "ash-twin": { x: 890, y: 295, r: 110, skip: [[62, 161]] }, // Ember Twin
        "ember-twin": { x: 840, y: 405, r: 110, skip: [[245, 347]] }, // Ash Twin
        "timber-hearth": { x: 575, y: 770, r: 200, skip: [[123, 162]] }, // Attlerock
        "brittle-hollow": { x: 1497, y: 484, r: 210, skip: [[293, 346]] }, // Hollow's Lantern
        "giants-deep": { x: 1464, y: 1104, r: 280, skip: [[67, 102]] }, // Orbital Probe Cannon
        "dark-bramble": { x: 2187, y: 1075, r: 160 }
    })
}
