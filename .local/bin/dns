#!/bin/bash

ip=$(dig -4 @resolver1.opendns.com ANY myip.opendns.com +short)
domain_ip=$(getent hosts runarsf.dev | awk '{ print $1 }')

echo "Public IP: ${ip}"
echo "Domain IP: ${domain_ip}"

if ! [[ "${ip}" = "${domain_ip}" ]]; then
  echo bing
fi