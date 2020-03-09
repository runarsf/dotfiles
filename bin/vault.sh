#!/bin/bash
# arg 1: vaultfile name
if test -z "${1}"; then
  vaultdir="${HOME}/.vault"
  vaultfile="${vaultdir}/.vault"
else
  vaultfile="$(readlink -f ${1})"
  vaultdir="$(dirname ${vaultfile})"
fi
cryptofile="${vaultfile}.aes"

printf "vaultdir: ${vaultdir}\n"
printf "vaultfile: ${vaultfile}\n"
printf "cryptofile: ${cryptofile}\n\n"

prompt () {
  printf "${1} [y/N]"
  read -p " " -n 1 -r </dev/tty
  printf "\n"
  if [[ ! "${REPLY}" =~ ^[Yy]$ ]]; then
    return 1
  fi
}

if test -z "${EDITOR}"; then
  if prompt "\$EDITOR not set, consider running \`EDITOR=vim ${0}\`. Run with vim?"; then
    EDITOR="vim"
  else
    printf "No editor set. Aborting.\n"
    exit 1
  fi
fi

encrypt () {
  # aescrypt -e -p "${password}" "${vaultfile}" -o "${cryptofile}"
  local input="${1}"
  local output="${2}"
  local password="${3}"
  gpg \
    --symmetric \
    --yes \
    --batch \
    --cipher-algo aes256 \
    --passphrase "${password}" \
    --output "${output}" \
    "${input}"
    #--pinentry-mode loopback \
}

decrypt () {
  # aescrypt -d -p "${password}" "${cryptofile}" -o "${vaultfile}"
  local input="${1}"
  local output="${2}"
  local password="${3}"
  gpg \
    --decrypt \
    --yes \
    --batch \
    --cipher-algo aes256 \
    --passphrase "${password}" \
    --output "${output}" \
    "${input}"
   #--pinentry-mode loopback \
}

if test -f "${vaultfile}"; then
  printf "The unencrypted vaultfile was found, this is bad!\n"
  printf "Either someone is using the file or something has gone wrong.\n"
  printf "Type 'w' to see if someone is using the file.\n"
  prompt "Show active users?" && printf '%s\n' "$(w)"
fi

read -p "Enter decryption password > " -s password
printf "\n"

if test -z "${password}"; then
  prompt "Password not entered, continue without password?" || exit 1
else
  printf "Password entered. Proceeding with decryption.\n"
fi

if test ! -f "${cryptofile}"; then
  printf "Cryptofile not detected, verifying prerequisites and creating.\n"
  if test ! -d "${vaultdir}"; then
    printf "Vault directory not detected, creating.\n"
    mkdir -p "${vaultdir}"
  else
    if test -d "${vaultdir}/.git"; then
      printf "Git directory detected, attempting checkout.\n"
      git -C "${vaultdir}" checkout "${cryptofile}"
    fi
  fi
  if test ! -f "${cryptofile}"; then
    if test ! -f "${vaultfile}"; then
      printf "Vaultfile not detected, creating dummy file.\n"
      printf "Unencrypted vaultfile." >> "${vaultfile}"
    fi
    encrypt "${vaultfile}" "${cryptofile}" "${password}"
    printf "Vault has been encrypted.\n"
    rm -f "${vaultfile}"
  fi
fi

decrypt "${cryptofile}" "${vaultfile}" "${password}"

if test ! -f "${vaultfile}"; then
  printf "The key was not found. Wrong password?\n"
  exit 1
else
  chmod 600 "${vaultfile}"
  premd5=$(md5sum -b "${vaultfile}" | awk '{print $1}')
  printf "Vault decrypted, opening in ${EDITOR}.\n"
  printf "Remember to verify that the file was successfully reencrypted.\n"
  #sleep 1

  eval "${EDITOR} ${vaultfile}"
  encrypt "${vaultfile}" "${cryptofile}" "${password}"
  printf "Vault has been reencrypted.\n"
fi

if test -f "${vaultfile}"; then
  postmd5=$(md5sum -b "${vaultfile}" | awk '{print $1}')
  rm -f "${vaultfile}"
fi

if test "${premd5}" = "${postmd5}"; then
  printf "File unencrypted. Aborting commit.\n"
else
  if prompt "Changes to crypto detected. Commit to revision system?"; then
    printf "Proceeding with checkin.\n"
    git -C "${vaultdir}" add "${cryptofile}"
    printf "Reason for change? > "
    read commitmsg
    git -C "${vaultdir}" commit -m "${commitmsg}"
    git -C "${vaultdir}" push origin master
  else
    printf "Aborting revision checkin.\n"
    #sleep 3
  fi
fi
