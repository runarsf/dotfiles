#!/usr/bin/env python

from fontTools.ttLib import TTFont
import sys

def rename_font(input_path, output_path, new_name):
    font = TTFont(input_path)
    for record in font['name'].names:
        if record.nameID in [1, 4, 6]:  # Family name, Full name, PostScript name
            record.string = new_name.encode(record.getEncoding())
    font.save(output_path)

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: rename_font.py <input_path> <output_path> <new_name>")
        sys.exit(1)
    rename_font(sys.argv[1], sys.argv[2], sys.argv[3])
