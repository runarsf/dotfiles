#!/usr/bin/env sh
test -L "$1" \
  && cp --remove-destination "$(readlink "$1")" "$1" \
  || echo "$1: Not a symlink."

