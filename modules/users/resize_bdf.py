#!/usr/bin/env python
import argparse
import numpy as np

# Lines that need all numbers scaled
scale_lines = [
    "SIZE",
    "FONTBOUNDINGBOX",
    "PIXEL_SIZE",
    "POINT_SIZE",
    "AVERAGE_WIDTH",
    "FONT_ASCENT",
    "FONT_DESCENT",
    "UNDERLINE_POSITION",
    "UNDERLINE_THICKNESS",
    "X_HEIGHT",
    "CAP_HEIGHT",
    "RAW_ASCENT",
    "RAW_DESCENT",
    "NORM_SPACE",
    "SUPERSCRIPT_",
    "SUBSCRIPT_",
    "FIGURE_WIDTH",
    "AVG_LOWERCASE_WIDTH",
    "AVG_UPPERCASE_WIDTH",
    "DWIDTH",
    "BBX",
    "QUAD_WIDTH",
]

def main():
    parser = argparse.ArgumentParser(description='Scale a BDF font by a given factor.')
    parser.add_argument('input_file', help='Input BDF font file')
    parser.add_argument('output_file', help='Output BDF font file')
    parser.add_argument('scale_factor', type=float, help='Scaling factor (e.g., 1.5)')

    args = parser.parse_args()

    with open(args.input_file, 'r') as src, open(args.output_file, 'w') as out:
        scale_font(src, out, args.scale_factor)

def scale_font(src, out, scale_factor):
    in_char = False
    collecting_bitmap = False
    bitmap_data = []
    char_lines = []
    char_properties = {}

    for line in src:
        if line.startswith("STARTCHAR"):
            in_char = True
            collecting_bitmap = False
            bitmap_data = []
            char_lines = [line]
            char_properties = {}
        elif line.startswith("ENDCHAR"):
            in_char = False
            collecting_bitmap = False
            # Process and write the character
            scaled_char_lines = process_char(char_lines, char_properties, bitmap_data, scale_factor)
            out.writelines(scaled_char_lines)
        elif in_char:
            char_lines.append(line)
            if line.startswith("BITMAP"):
                collecting_bitmap = True
            elif collecting_bitmap:
                if line.strip() == "ENDCHAR":
                    collecting_bitmap = False
                else:
                    bitmap_data.append(line.strip())
            else:
                # Collect character properties
                if ' ' in line:
                    key, *values = line.strip().split()
                    char_properties[key] = values
        else:
            # Not in character, process global properties
            # Scale global properties if needed
            if any(line.startswith(x) for x in scale_lines):
                words = line.strip().split()
                key = words[0]
                scaled_values = []
                for value in words[1:]:
                    try:
                        num = float(value)
                        num_scaled = num * scale_factor
                        scaled_values.append(str(int(round(num_scaled))))
                    except ValueError:
                        scaled_values.append(value)
                line = ' '.join([key] + scaled_values) + '\n'
            elif line.startswith("FONT "):
                # Handle the FONT line
                xlfd = line[len("FONT "):].strip().split("-")
                # PIXEL_SIZE is field 7
                xlfd[7] = str(int(round(float(xlfd[7]) * scale_factor)))
                # POINT_SIZE is field 8
                xlfd[8] = str(int(round(float(xlfd[8]) * scale_factor)))
                # AVERAGE_WIDTH is field 12
                xlfd[12] = str(int(round(float(xlfd[12]) * scale_factor)))
                line = "FONT " + "-".join(xlfd) + "\n"
            # Replace font name if needed
            line = line.replace("Cozette", "CozetteHiDpi")
            out.write(line)
    
def process_char(char_lines, char_properties, bitmap_data, scale_factor):
    # Update the properties
    scaled_char_lines = []
    for line in char_lines:
        if any(line.startswith(x) for x in ["BBX", "DWIDTH", "SWIDTH"]):
            words = line.strip().split()
            key = words[0]
            scaled_values = []
            for value in words[1:]:
                try:
                    num = float(value)
                    num_scaled = num * scale_factor
                    scaled_values.append(str(int(round(num_scaled))))
                except ValueError:
                    scaled_values.append(value)
            scaled_line = ' '.join([key] + scaled_values) + '\n'
            scaled_char_lines.append(scaled_line)
        elif line.strip() == "BITMAP":
            scaled_char_lines.append("BITMAP\n")
            # Now, process the bitmap data
            scaled_bitmap_data = scale_bitmap(bitmap_data, scale_factor)
            for bitmap_line in scaled_bitmap_data:
                scaled_char_lines.append(bitmap_line + '\n')
        else:
            scaled_char_lines.append(line)
    scaled_char_lines.append("ENDCHAR\n")
    return scaled_char_lines

def scale_bitmap(bitmap_data, scale_factor):
    # Convert bitmap data to 2D array of bits
    bit_rows = []
    for hex_line in bitmap_data:
        hex_line = hex_line.strip()
        # Convert hex to binary string
        num_bits = len(hex_line) * 4  # Each hex digit represents 4 bits
        bin_line = bin(int(hex_line, 16))[2:].zfill(num_bits)
        bit_row = [int(b) for b in bin_line]
        bit_rows.append(bit_row)
    bitmap_array = np.array(bit_rows, dtype=np.uint8)
    # Now, scale the bitmap array
    height, width = bitmap_array.shape
    new_height = int(round(height * scale_factor))
    new_width = int(round(width * scale_factor))
    if new_height == 0:
        new_height = 1
    if new_width == 0:
        new_width = 1
    scaled_bitmap = scale_array(bitmap_array, new_height, new_width)
    # Convert scaled bitmap back to hex lines
    scaled_bitmap_data = []
    for row in scaled_bitmap:
        bin_str = ''.join(str(bit) for bit in row)
        # Pad to multiple of 8 bits
        pad_length = (8 - len(bin_str) % 8) % 8
        bin_str_padded = bin_str.ljust(len(bin_str) + pad_length, '0')
        # Now, group bits into bytes (8 bits)
        hex_line = ''
        for i in range(0, len(bin_str_padded), 8):
            byte_str = bin_str_padded[i:i+8]
            hex_byte = hex(int(byte_str, 2))[2:].upper().zfill(2)
            hex_line += hex_byte
        scaled_bitmap_data.append(hex_line)
    return scaled_bitmap_data

def scale_array(array, new_height, new_width):
    height, width = array.shape
    scale_y = height / new_height
    scale_x = width / new_width
    scaled_array = np.zeros((new_height, new_width), dtype=np.uint8)
    for y in range(new_height):
        orig_y = int(y * scale_y)
        if orig_y >= height:
            orig_y = height - 1
        for x in range(new_width):
            orig_x = int(x * scale_x)
            if orig_x >= width:
                orig_x = width - 1
            scaled_array[y, x] = array[orig_y, orig_x]
    return scaled_array

if __name__ == '__main__':
    main()
