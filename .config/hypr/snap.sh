#!/usr/bin/env bash

# Automatically snaps the floating window to the nearest edge of a tiled window.

"${HOME}/.config/hypr/onscreen.sh"

client="$(hyprctl -j activewindow)"
workspace_id="$(hyprctl -j activeworkspace | jq -r '.id')"
client_id="$(printf '%s\n' "${client}" | jq -r '.pid')"
clients="$(hyprctl -j clients | jq -r ".[] | select(.workspace.id == ${workspace_id}) | select(.pid != ${client_id})")"

# How far away a window should snap
gravity="50"

client_n="$(printf '%s\n' "${client}" | jq -r '.at[1]')"
client_s="$(printf '%s\n' "${client}" | jq -r '.at[1] + .size[1]')"
client_w="$(printf '%s\n' "${client}" | jq -r '.at[0]')"
client_e="$(printf '%s\n' "${client}" | jq -r '.at[0] + .size[0]')"

# Get clients for each direction where the active client is within the bounds
declare -a clients_n=( $(printf '%s\n' "${clients}" | jq -r "select(${client_w} <= .at[0] + .size[0]) | select(${client_w} >= .at[0]) | .at[1]") )
declare -a clients_s=( $(printf '%s\n' "${clients}" | jq -r "select(${client_w} <= .at[0] + .size[0]) | select(${client_w} >= .at[0]) | .at[1] + .size[1]") )
declare -a clients_w=( $(printf '%s\n' "${clients}" | jq -r "select(${client_n} <= .at[1] + .size[1]) | select(${client_n} >= .at[1]) | .at[0]") )
declare -a clients_e=( $(printf '%s\n' "${clients}" | jq -r "select(${client_n} <= .at[1] + .size[1]) | select(${client_n} >= .at[1]) | .at[0] + .size[0]") )

for pos_n in "${clients_n[@]}"; do
  diff="$(( pos_n - client_n ))"
  : "${snap_n:=${diff}}"

  if test "${diff#-}" -lt "${snap_n#-}"; then
    snap_n="${diff}"
  fi
done

for pos_s in "${clients_s[@]}"; do
  diff="$(( pos_s - client_s ))"
  : "${snap_s:=${diff}}"

  if test "${diff#-}" -lt "${snap_s#-}"; then
    snap_s="${diff}"
  fi
done

if test "${snap_n#-}" -le "${snap_s#-}"; then
  snap_y="${snap_n}"
else
  snap_y="${snap_s}"
fi
if test "${snap_y#-}" -gt "${gravity#-}"; then
  snap_y="0"
fi



for pos_w in "${clients_w[@]}"; do
  diff="$(( pos_w - client_w ))"
  : "${snap_w:=${diff}}"

  if test "${diff#-}" -lt "${snap_w#-}"; then
    snap_w="${diff}"
  fi
done

for pos_e in "${clients_e[@]}"; do
  diff="$(( pos_e - client_e ))"
  : "${snap_e:=${diff}}"

  if test "${diff#-}" -lt "${snap_e#-}"; then
    snap_e="${diff}"
  fi
done

if test "${snap_w#-}" -le "${snap_e#-}"; then
  snap_x="${snap_w}"
else
  snap_x="${snap_e}"
fi
if test "${snap_x#-}" -gt "${gravity#-}"; then
  snap_x="0"
fi

if test "${snap_x}" -ne "0" -o "${snap_y}" -ne "0"; then
  hyprctl dispatch movewindowpixel -- "${snap_x}" "${snap_y}",pid:"${client_id}"
fi