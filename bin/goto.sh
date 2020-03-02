#!/usr/bin/env sh
test -L "$1" \
  && cd $(dirname $(readlink -f "$1")) \
  || echo "$1: Not a symlink."
