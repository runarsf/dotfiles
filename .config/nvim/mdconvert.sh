#!/bin/sh
# https://alexb7711.github.io/Markdown-Live-Preview-Without-Vim-Pluggins/
# requires pdflatex, texlive-most on arch
file="$(readlink -f "${1}")"
dir="$(dirname "${file}")"
base="${file%.*}"
shebang="$(sed -n 1p "${file}")"

cd "${dir}" || exit

case "${file}" in
    *.md) pandoc -f markdown -t pdf -o "${base}.pdf" "${file}";;
esac

if ! pgrep "zathura" >/dev/null 2>&1; then
  zathura "${base}.pdf" &
  disown
fi
