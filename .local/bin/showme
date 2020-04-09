#!/usr/bin/env sh
dir="${1}"
term="${2}"
if test ! -e "${dir}"; then
  term="${dir}"
  dir="."
fi
grep -winR "${dir}" -e "${term}"
