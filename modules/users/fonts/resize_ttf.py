#!/usr/bin/env python

from fontTools.ttLib import TTFont
from fontTools.ttLib.tables._g_l_y_f import GlyphCoordinates
import sys
import os


def resize_font(input_path, output_path, scale_factor):
    # Load the font
    font = TTFont(input_path)

    # Scale the font metrics
    for table in ["hhea", "OS/2", "head"]:
        if table in font:
            if table == "hhea":
                font[table].ascent = int(font[table].ascent * scale_factor)
                font[table].descent = int(font[table].descent * scale_factor)
                font[table].lineGap = int(font[table].lineGap * scale_factor)
            elif table == "OS/2":
                font[table].sTypoAscender = int(
                    font[table].sTypoAscender * scale_factor
                )
                font[table].sTypoDescender = int(
                    font[table].sTypoDescender * scale_factor
                )
                font[table].sTypoLineGap = int(font[table].sTypoLineGap * scale_factor)
                font[table].usWinAscent = int(font[table].usWinAscent * scale_factor)
                font[table].usWinDescent = int(font[table].usWinDescent * scale_factor)
                font[table].sxHeight = int(font[table].sxHeight * scale_factor)
                font[table].sCapHeight = int(font[table].sCapHeight * scale_factor)
            elif table == "head":
                font[table].unitsPerEm = int(font[table].unitsPerEm * scale_factor)

    # Scale the glyphs
    glyf = font["glyf"]
    for glyph_name in glyf.keys():
        glyph = glyf[glyph_name]
        if glyph.isComposite():
            # Scale composite glyph components
            for component in glyph.components:
                component.x = int(component.x * scale_factor)
                component.y = int(component.y * scale_factor)
        else:
            # Scale simple glyph coordinates
            if hasattr(glyph, "coordinates"):
                coords = glyph.coordinates
                new_coords = []
                for x, y in coords:
                    new_coords.append((int(x * scale_factor), int(y * scale_factor)))
                glyph.coordinates = GlyphCoordinates(new_coords)

    # Scale the horizontal metrics
    if "hmtx" in font:
        for name in font["hmtx"].metrics:
            font["hmtx"].metrics[name] = (
                int(font["hmtx"].metrics[name][0] * scale_factor),
                int(font["hmtx"].metrics[name][1] * scale_factor),
            )

    # Save the modified font
    font.save(output_path)


def main():
    if len(sys.argv) != 4:
        print("Usage: resize_ttf.py input.ttf output.ttf scale_factor")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    try:
        scale_factor = float(sys.argv[3])
    except ValueError:
        print("Error: scale_factor must be a number")
        sys.exit(1)

    if not os.path.exists(input_path):
        print(f"Error: Input file '{input_path}' does not exist")
        sys.exit(1)

    if os.path.exists(output_path):
        os.remove(output_path)

    try:
        resize_font(input_path, output_path, scale_factor)
        print(f"Successfully resized font by factor {scale_factor}")
    except Exception as e:
        print(f"Error: {str(e)}")
        sys.exit(1)


if __name__ == "__main__":
    main()
