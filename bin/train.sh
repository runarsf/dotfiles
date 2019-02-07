#!/bin/bash
trap "exit" INT
while :
do
	clear
	sl -e | lolcat
done
