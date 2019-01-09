#!/bin/bash
trap "exit" INT
while :
do
    sl -e | lolcat
done
