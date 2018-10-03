#!/bin/bash

while getopts ":" opt; do
	case $opt in
		l)
		;;
		\?)
			printf "Invalid option: -$OPTARG"
			;;
	esac
done
