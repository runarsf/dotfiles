#!/bin/bash

echo $(date)
echo ""

read -p "Hvor mange var på lager etter forrige påfyll? " -r </dev/tty
PREV=$REPLY

read -p "Hvor mange er kvittert? " -r </dev/tty
RECEIPT=$REPLY

read -p "Hvor mange er på lager nå? " -r </dev/tty
BEFORE=$REPLY

printf "Avvik: $(($PREV-($RECEIPT+$BEFORE)))\n"

read -p "Hvor mange ble fylt på? " -r </dev/tty
FILL=$REPLY

printf "Antall etter påfyll: $(($BEFORE+$FILL))\n"

