#!/usr/bin/env bash
# ███╗███╗
# ██╔╝╚██║
# ██║  ██║
# ██║  ██║
# ███╗███║
# ╚══╝╚══╝
# ╔══╗███████╗ ██████╗╔══╗██████╗ ███████╗███████╗███╗   ██╗   ╔══╗███████╗██╗  ██╗╔══╗ ██████╗ ████████╗
# ║ ╔╝██╔════╝██╔════╝╚╗ ║██╔══██╗██╔════╝██╔════╝████╗  ██║   ║ ╔╝██╔════╝██║  ██║╚╗ ║██╔═══██╗╚══██╔══╝
# ║ ║ ███████╗██║      ║ ║██████╔╝█████╗  █████╗  ██╔██╗ ██║   ║ ║ ███████╗███████║ ║ ║██║   ██║   ██║
# ║ ║ ╚════██║██║      ║ ║██╔══██╗██╔══╝  ██╔══╝  ██║╚██╗██║   ║ ║ ╚════██║██╔══██║ ║ ║██║   ██║   ██║
# ║ ╚╗███████║╚██████╗╔╝ ║██║  ██║███████╗███████╗██║ ╚████║██╗║ ╚╗███████║██║  ██║╔╝ ║╚██████╔╝   ██║
# ╚══╝╚══════╝ ╚═════╝╚══╝╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═══╝╚═╝╚══╝╚══════╝╚═╝  ╚═╝╚══╝ ╚═════╝    ╚═╝

test -z "${DISPLAY}" && { printf "X server not running.\n"; exit 1; }
requirements=(import jq curl xclip)
for cmd in "${requirements[@]}"; do command -v "${cmd}" >/dev/null 2&>1 || { printf "'${cmd}' required to run.\n"; exit 1; }; done

#if test -n "${SCSH_USE_SHARENIX_CONFIG:=}"; then
#  if test ! -f "${HOME}/.sharenix.json"; then
#    printf "Sharenix config not found. Consider unsetting \$SCSH_USE_SHARENIX_CONFIG."
#    exit 1
#  fi
#  sharenix_config="$(cat ${HOME}/.sharenix.json)"
#
#  SCSH_REQUEST_TYPE=$(jq --raw-output '.Services[0].RequestType' <<< "${sharenix_config}")
#  SCSH_REQUEST_URL=$(jq --raw-output '.Services[0].RequestURL' <<< "${sharenix_config}")
#  SCSH_FILE_FORM_NAME=$(jq --raw-output '.Services[0].FileFormName' <<< "${sharenix_config}")
#  SCSH_HEADERS=$(jq --raw-output '.Services[0].Headers' <<< "${sharenix_config}")
#fi
#echo $SCSH_REQUEST_TYPE
#exit

#test -f "${SCSH_CONFIG:=${HOME}/.sc.sh}" && source "${SCSH_CONFIG}"
#: ${SCSH_REQUEST_URL:=}
#: ${SCSH_REQUEST_TOKEN:=}
#: ${SCSH_TEMP_FILE:=/tmp/scsh.png}
#: ${SCSH_REQUEST_TYPE:=POST}
#: ${SCSH_REQUEST_FILE_FORM_NAME:=files[]}
#: ${SCSH_JSON_FILTER:=.files[0].url}

import "${SCSH_TEMP_FILE}"

xclip -selection clipboard -t image/png -i "${SCSH_TEMP_FILE}"

  #--silent \
  #--show-error \
  #--output /dev/null \
  #--connect-timeout 15 \
  #--max-time 30 \
data=$(\curl \
  "${SCSH_REQUEST_URL}" \
  --silent \
  --fail \
  --location \
  --compressed \
  --request "${SCSH_REQUEST_TYPE}" \
  --form "${SCSH_REQUEST_FILE_FORM_NAME}=@${SCSH_TEMP_FILE}" \
  --header 'Accept: application/json' \
  --header "token: ${SCSH_REQUEST_TOKEN}" \
  --header 'Content-Type: multipart/form-data')

jq --raw-output "${SCSH_JSON_FILTER}" <<< "${data}"
