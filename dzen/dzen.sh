#!/bin/bash
# Modified from HerbstluftWM's panel.sh

#======~===~==============~===========~==
# COLORS
#==~==========~=========~=============~~=

# Getting colors from `xrdb -query`
if [[ $(uname) == 'FreeBSD' ]]; then
	xrdb=( $(xrdb -query | grep "color[0-9]*:" | sort | cut -f 2-) )
else
	xrdb=( $(xrdb -query | grep -P "color[0-9]*:" | sort | cut -f 2-) )
fi
declare -A color
index=0
for name in black brightgreen brightyellow brightblue brightmagenta brightcyan brightwhite red green yellow blue magenta cyan white grey brightred; do
	color[${name}]=${xrdb[$index]}
	((index++))
done
bgcolor='#1f1b18'

#======~===~==============~===========~==
# GEOMETRY
#==~==========~=========~=============~~=
x=0
y=0
width=1920
height=20

font="Pragmata:size=10"
bold="Pragmata:style=Bold:size=10"

# Using a different font to calculate text width (`textwidth` doesn't work with truetype fonts)
# Neep Semicondensed 11 has the same char width as Pragmata 7pt
calcfont="-jmk-neep-medium-r-semicondensed--11-100-75-75-c-50-iso8859-9"

#======~===~==============~===========~==
# ICONS
#==~==========~=========~=============~~=
iconpath=${XDG_CONFIG_HOME}/stumpwm/dzen/icons
function icon() {
	echo -n "^fg(${color[${2}]})^i(${iconpath}/${1}.xbm)^fg()"
}

#======~===~==============~===========~==
# IRSSI
#==~==========~=========~=============~~=
irssilog=${XDG_DATA_HOME}/log/irssi/hilights.log
function irssi() {
	if [[ -f ${irssilog} ]]; then
		lastline=$(tail -n1 ${irssilog})
		echo -n $(icon balloon red) $(echo -n ${lastline} | cut -d " " -f -3)
	fi
}

#======~===~==============~===========~==
# CPU
#==~==========~=========~=============~~=
function temperature() {
	cpu=$(sensors | grep -P "(temp1)|(Core)" | cut -b 16-19)
	echo -n $(icon temp yellow) ${cpu}
}

#======~===~==============~===========~==
# MPD
#==~==========~=========~=============~~=
function m() {
	mpc -f %${1}% current | sed 's/ä/ae/g' | sed 's/ö/oe/g' | sed 's/ü/ue/g'
}

function nowplaying() {
	echo -n "$(icon note1 magenta) $(m title) ^fg(#909090)by^fg() $(m artist)"
}

function uniq_linebuffered() {
	awk '$0 != l { print ; l=$0 ; fflush(); }' "$@"
}

{
	tail -f ~/var/log/stumpwm.log &
	tail -f ${irssilog} &
   mpc idleloop player &
	while true ; do
		date +'date ^fg(#efefef) %H:%M^fg(#909090), %Y-%m-^fg(#efefef)%d'
		sleep 1 || break
	done > >(uniq_linebuffered)  &
	while true ; do
		echo "vol $(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')"
		sleep 1 || break
	done > >(uniq_linebuffered) &
} 2> /dev/null | {
	while :; do

		ws=$(~/.stumpwm.d/dzen/groups.sh)
		echo -n " ^fn($bold)$ws^fn()"

		right=""
		for func in nowplaying irssi temperature; do
			right="$right $(${func})"
		done

		right="$right $(icon volume_on blue) $volume"

		# Date
		right="$right $(icon clock1 green)^fn($bold)$date^fn()"
		right_text_only=$(echo -n "$right"|sed 's.\^[^(]*([^)]*)..g')

		# get width of right aligned text.. and add some space..
		width=$(textwidth $calcfont "$right_text_only            ")
		echo -n "^p(_RIGHT)^p(-$width)$right"
		echo

		# wait for next event
		read line || break
		cmd=( $line )

		# find out event origin
		case "${cmd[0]}" in
			date)
				date="${cmd[@]:1}" ;;
			vol)
				volume="${cmd[@]:1}" ;;
		esac
	done
#} 2> /dev/null | dzen2 -w $width -x $x -y $y -fn "$font" -h $height \
#	-ta l -bg "$bgcolor" -fg '#efefef'
} 2> /dev/null | dzen2 -w $width -x $x -y $y -fn "$font" -h $height \
	-ta l -bg "$bgcolor" -fg '#efefef'
