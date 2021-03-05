import * as React from "react"
import * as Oni from "oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
    console.log("config activated")

    // Input
    //
    // Add input bindings here:
    //
    oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))

    //
    // Or remove the default bindings here by uncommenting the below line:
    //
    // oni.input.unbind("<c-p>")

}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log("config deactivated")
}

export const configuration = {
    //"ui.colorscheme": "n/a",
    "ui.colorscheme": "one",
    "autoClosingPairs.enabled": false, // handled by CoC
    "editor.textMateHighlighting.enabled": true, // Use vim syntax highlighting
    "achievements.enabled": false,
    //"debug.persistOnNeovimExit": false,
    "sidebar.enabled": true,
    "sidebar.default.open": false,
    "learning.enabled": false,

    "editor.fontSize": "14px",
    "editor.fontFamily": "Source Code Pro",
    "editor.fontLigatures": false,
    "editor.linePadding": 1,

    "editor.renderer": "canvas",
    "ui.animations.enabled": true,
    "ui.fontSmoothing": "auto"
}
