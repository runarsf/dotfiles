import QtQuick
import Quickshell
import Quickshell.Io

ShellRoot {
    id: root

    // A spot is { screen, planet, angle }: a screen name, a key of
    // Config.planets (or "eye"), and an angle on that planet's ring.
    readonly property var eye: ({ screen: "", planet: "eye", angle: 0 })
    property var moon: eye
    property bool wasCovered: true
    property string lastWindows: ""

    // Swap this for another backend to support a different compositor.
    HyprlandWindows { id: windows }

    Variants {
        model: Quickshell.screens
        Wallpaper {
            moon: root.moon.screen === modelData.name ? root.sprite(root.moon) : null
        }
    }

    // Act once windows have stopped changing for Config.delay ms: lets bursts
    // of updates and window animations finish first.
    Timer {
        id: settle
        interval: Config.delay
        onTriggered: root.evaluate()
    }

    Connections {
        target: windows
        function onRectsChanged() { settle.restart(); }
    }

    Connections {
        target: Quickshell
        function onScreensChanged() {
            if (root.moon.planet !== "eye" && !root.screenNamed(root.moon.screen))
                root.place(root.randomSpot());
        }
    }

    // qs -c quantum-moon ipc call moon move
    // qs -c quantum-moon ipc call moon moveTo giants-deep
    IpcHandler {
        target: "moon"

        function move(): void {
            root.place(root.next(true));
        }

        function moveTo(location: string): string {
            if (location === "eye")
                root.place(root.eye);
            else if (location in Config.planets)
                root.place(root.randomSpot([location]));
            else
                return `unknown location, use one of: eye, ${Object.keys(Config.planets).join(", ")}`;
            return "";
        }
    }

    Component.onCompleted: place(randomSpot())

    // --- Rules -------------------------------------------------------------

    // The moon jumps the moment it becomes fully covered. From the Eye it
    // returns as soon as any spot is covered.
    function evaluate() {
        const key = JSON.stringify(windows.rects);
        if (key === lastWindows)
            return;
        lastWindows = key;

        const isCovered = covered(moon);
        if (moon.planet === "eye" || (isCovered && !wasCovered))
            place(next(false));
        else
            wasCovered = isCovered;
    }

    // Where the moon goes next. `force` ignores observation (manual move).
    function next(force) {
        const here = moon.planet;
        if (here !== "eye" && Math.random() < Config.eyeChance)
            return eye;

        const all = spots();
        if (force)
            return pick(all.filter(s => s.planet !== here));

        const hidden = all.filter(covered);
        return pick(hidden.filter(s => s.planet !== here)) ?? pick(hidden) ?? moon;
    }

    function place(spot) {
        moon = spot;
        wasCovered = covered(spot);
    }

    // --- Spots -------------------------------------------------------------

    // Candidate spots: every screen × planet × a jittered sample of allowed angles.
    function spots() {
        const out = [];
        for (const screen of allScreens())
            for (const planet in Config.planets)
                for (let i = 0; i < Config.samples; i++)
                    out.push({ screen: screen.name, planet, angle: 2 * Math.PI * (i + Math.random()) / Config.samples });
        return out.filter(allowed);
    }

    function randomSpot(planets = Object.keys(Config.planets)) {
        return pick(spots().filter(s => planets.includes(s.planet))) ?? eye;
    }

    function allowed(spot) {
        const deg = spot.angle * 180 / Math.PI;
        return !(Config.planets[spot.planet].skip ?? []).some(([from, to]) =>
            from <= to ? deg >= from && deg <= to : deg >= from || deg <= to);
    }

    // --- Geometry ----------------------------------------------------------

    // Moon center and rotation in image pixels, lit side facing the sun.
    function sprite(spot) {
        const p = Config.planets[spot.planet];
        const x = Math.round(p.x + p.r * Math.cos(spot.angle));
        const y = Math.round(p.y + p.r * Math.sin(spot.angle));
        return { x, y, rotation: towardSun(x, y) - towardSun(Config.moonOrigin.x, Config.moonOrigin.y) };
    }

    function towardSun(x, y) {
        return Math.atan2(Config.sun.y - y, Config.sun.x - x) * 180 / Math.PI;
    }

    // Moon bounding box in global logical coordinates (image scaled to cover, centered).
    function rectOf(spot) {
        const screen = screenNamed(spot.screen);
        const m = sprite(spot);
        const s = Math.max(screen.width / Config.imageWidth, screen.height / Config.imageHeight);
        const half = Config.moonSize / 2;
        return {
            x: screen.x + (screen.width - Config.imageWidth * s) / 2 + (m.x - half) * s,
            y: screen.y + (screen.height - Config.imageHeight * s) / 2 + (m.y - half) * s,
            w: Config.moonSize * s,
            h: Config.moonSize * s
        };
    }

    // Fully covered by the union of visible windows (gaps between tiles count as visible).
    function covered(spot) {
        if (spot.planet === "eye")
            return true;
        let rest = [rectOf(spot)];
        for (const w of windows.rects)
            rest = [].concat(...rest.map(r => subtract(r, w)));
        return rest.length === 0;
    }

    // a minus b, as up to four rectangles.
    function subtract(a, b) {
        const x1 = Math.max(a.x, b.x), y1 = Math.max(a.y, b.y);
        const x2 = Math.min(a.x + a.w, b.x + b.w), y2 = Math.min(a.y + a.h, b.y + b.h);
        if (x1 >= x2 || y1 >= y2)
            return [a];
        const out = [];
        if (a.y < y1) out.push({ x: a.x, y: a.y, w: a.w, h: y1 - a.y });
        if (y2 < a.y + a.h) out.push({ x: a.x, y: y2, w: a.w, h: a.y + a.h - y2 });
        if (a.x < x1) out.push({ x: a.x, y: y1, w: x1 - a.x, h: y2 - y1 });
        if (x2 < a.x + a.w) out.push({ x: x2, y: y1, w: a.x + a.w - x2, h: y2 - y1 });
        return out;
    }

    // --- Helpers -----------------------------------------------------------

    function allScreens() { return Array.from(Quickshell.screens); }
    function screenNamed(name) { return allScreens().find(s => s.name === name); }
    function pick(list) { return list[Math.floor(Math.random() * list.length)]; }
}
