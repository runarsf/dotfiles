#!/usr/bin/env bash

# A workaround because eww is yuck and doesn't allow starting windows on dynamically defined monitors.

CONFIG_DIR="${HOME}/.config/eww"
PRIMARY_BAR_WIDGET="bar"
ALT_BAR_WIDGET="alt-bar"
# To add default bars to open:
#  set -- "bar-1" "bar-2"

get_yuck() {
  monitor="${1:?}"
  widget="${2:?}"
  
	cat <<-EOF > /dev/stdout
	(defwindow bar-${monitor}
	  :monitor ${monitor}
	  :windowtype "dock"
	  :stacking "bg"
	  :wm-ignore false
	  :hexpand false
	  :vexpand false
	  :geometry (geometry :x "0%"
	                      :y "10px"
	                      :width "99%"
	                      :height "35px" ; Has to be odd for pixel-perfect vertical centering
	                      :anchor "top center")
	  :reserve (struts :side "top"
	                   :distance "45px")
	  (${widget}))
	EOF
}

rm -f "${CONFIG_DIR}/bars.yuck"

while read -r monitor; do
  _id="$(printf '%s' "${monitor}" | awk -F ':' '/1/ {print $1}')"
  if [[ "$(printf '%s' "${monitor}" | awk '{print $2}')" == *"*"* ]]; then
    _widget="${PRIMARY_BAR_WIDGET}"
  else
    _widget="${ALT_BAR_WIDGET}"
  fi
  
  printf '%s\n\n' "$(get_yuck "${_id}" "${_widget}")" >> "${CONFIG_DIR}/bars.yuck"
  
  set -- "${@}" "bar-${_id}"
done < <(xrandr --listmonitors | tail -n+2)

sleep 0.5
eww kill
eww daemon

while test "${#}" -gt "0"; do
  eww open "${1}"
  shift
done
