IP="${1:?Usage: ${0} <IP:PORT> <PROCESSOR?>}"
PROCESSOR="${2:-cat}" # tail -c 41 | base64 --decode
HOST="$(printf '%s' "${IP}" | awk -F: '{print $1}')"; : "${HOST:?Missing HOST}"
PORT="$(printf '%s' "${IP}" | awk -F: '{print $2}')"; : "${PORT:?Missing PORT}"

# Establish a connection and keep it open
exec 3<>/dev/tcp/"${HOST}"/"${PORT}"

# Read from the connection
cat <&3 | while read -r line; do
  # Decode the message
  decoded="$(printf '%s\n' "${line}" | "${PROCESSOR}")"

  printf 'Decoded message: %s\n' "${decoded}"

  # Send the response back through the same connection
  printf '%s\n' "${decoded}" >&3
done