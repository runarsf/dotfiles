#!/usr/bin/env bash
set -o errexit
set -o nounset

STATUS="$(playerctl status 2>/dev/null | tr '[:upper:]' '[:lower:]')"
PLAYER="$(playerctl -l 2>/dev/null | head -1 | cut -d. -f1)"

case "${STATUS}" in
  "paused");;
  "playing") STATUS="${PLAYER}";;
  *) exit 1;;
esac

ARTIST="$(playerctl metadata artist 2>/dev/null | head -1 | sed 's/&/&amp;/g')"
TITLE="$(playerctl metadata title 2>/dev/null | head -1 | sed 's/&/&amp;/g')"

printf '{ "text": "%s - %s", "class": "custom-%s", "alt": "%s" }\n' \
  "${ARTIST}" \
  "${TITLE}" \
  "${PLAYER}" \
  "${STATUS}"
