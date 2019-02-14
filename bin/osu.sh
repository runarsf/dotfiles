#!/bin/sh
export WINEPREFIX="$HOME/.wine_osu"
[ "$1" = "kill" ] && wineserver -k && echo 'wineserver killed' && exit 0
export STAGING_AUDIO_DURATION=5000 # As low as you can get osu! stable with

# Arch Linux/wine-osu users should uncomment next line
# for the patch to be effective
#export PATH=/opt/wine-osu/bin:$PATH

wineserver -k

cd ~/Games/osu # Or wherever you installed osu! in
wine osu!.exe "$@"

