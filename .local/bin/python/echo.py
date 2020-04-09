#!/usr/bin/env python3

import sys

if __name__ == "__main__":
    for line in sys.stdin:
        sys.stderr.write("DEBUG: got line: " + line)
        sys.stdout.write(line)
