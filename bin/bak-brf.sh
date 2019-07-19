#!/bin/bash

# > Increment pointer
# < Decrement pointer
# + Increment cell
# - Decrement cell
# . Output cell contents at pointer location
# , Input into cell at pointer location (one byte)
# [ If the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
# ] if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.

UP=$(tput cuu1)
ERASE=$(tput el)
SOURCE=$1
CELLS=()
P=0 # Pointer
NEST=()
FILE=$(cat $1 | tr -d " \t\n\r")

printm () {
	printf %${2}s |tr " " "${1}"
}

update () { # int
	DASHES=$(expr length "${1}")
	D=$(printm "-" $DASHES)
	DASHES=$(expr length "${2}")
	V=$(printm "-" $DASHES)

	printf "$UP$ERASE$UP$ERASE$UP$ERASE$UP$ERASE"
	printf "+$D+--$V+\n"
	printf "|$1| $2 |\n"
	printf "+$D+--$V+\n"
	printf ">\n"
}

init () {
	# Print a placeholder that will be replaced by the update function
	printf "\n+-+--+\n"
	printf "|$P|  |\n"
	printf "+-+--+\n"
	printf ">\n"

	parse
}

parse () {

	for (( i=0; i<${#SOURCE}; i++ )); do

		char="${SOURCE:(($i)):1}"

		# Set cell to 0 if no other value is set
		[ "${CELLS[$P]}" = "" ] && CELLS[$P]=0

		#from="$P|${CELLS[$P]}"

	    case $char in
			"+") CELLS[$P]=$((${CELLS[$P]}+1));;
			"-") CELLS[$P]=$((${CELLS[$P]}-1));;
			">") P=$((P+1));;
			"<") P=$((P-1));;
			".") printf "» ${CELLS[$P]}\n";;
			",") read -p "« " -r </dev/tty && CELLS[$P]=$REPLY;;
			"[") NEST[${#NEST}]=${i};;
			"]") [ "$DATA" = "0" ] && i=${NEST[$((${#NEST}+1))]} || NEST[${#NEST}]="";;
			*) continue;;
		esac
		DATA=${CELLS[$P]}
		#printf "$from => $P|${CELLS[$P]}\n"

	done

}

parse
