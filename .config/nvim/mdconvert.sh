#!/bin/sh

file=$(readlink -f "${1}")
dir=$(dirname "${file}")
base="${file%.*}"
shebang=$(sed -n 1p "${file}")

cd "${dir}" || exit

case "${file}" in
    *.md) pandoc "${file}" -o "${base}".pdf;; 
esac

