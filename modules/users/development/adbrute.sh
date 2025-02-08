IP="${1:?Usage: ${0} <IP>}"
PORT="${2:-}"

if [[ -n "${PORT// }" ]]; then
  timeout 2.5s adb connect "${IP}:${PORT}"
  exit ${?}
fi

if [[ "${IP}" != *"."* ]]; then
  HOST_IP="$(ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p' | head -1)"
  IP="$(printf '%s\n' "${HOST_IP}" | awk -v IP="${IP}" -F'.' '{print $1"."$2"."$3"."IP}')"
  printf 'Only the last octet was provided, assuming %s...\n' "${IP}"
fi

printf 'Scanning for open ports on %s...\n' "${IP}"
PORTS="$(nmap "${IP}" -p 30000-50000 | awk '/\/tcp/' | cut -d/ -f1)"

while IFS= read -r port; do
  if [[ -z "${port// }" ]]; then
    printf 'No open ports found.\n'
    break
  fi

  printf 'Trying %s:%s...\n' "${IP}" "${port}"

  if timeout 2.5s adb connect "${IP}:${port}"; then
    break
  fi
done <<< "${PORTS}"
