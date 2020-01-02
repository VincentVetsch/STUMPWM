#!/bin/bash

current='^bg(#efaf63)^fg(#332d29)'
last='^bg(#75715e)^fg(#332d29)'
other='^bg(#26221f)'

current2='^bg(#f9cda2)^fg(#64657b)'
last2='^bg(#64657b)^fg(#f9cda2)'
other2='^bg(#332d29)'

groups=$(stumpish groups)

for g in $groups; do
	n=$(echo $g | sed -e "s/^\([0-9]\).*/\1/")
	group=$(echo $g | sed -e "s/^[0-9]//")
	name=$(echo $group | sed -e "s/^[-+*]//")
	format1=$(echo $group | sed \
		-e "s/^-.*/$other /" \
		-e "s/^\+.*/$last /" \
		-e "s/^\*.*/$current /")
	format2=$(echo $group | sed \
		-e "s/^-.*/$other2 /" \
		-e "s/^\+.*/$last2 /" \
		-e "s/^\*.*/$current2 /")
	echo -n "^ca(1,stumpish go-group '$n')$format2$n $format1$name ^fg()^bg()^ca() "
done
