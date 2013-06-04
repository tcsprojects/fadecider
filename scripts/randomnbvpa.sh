#!/bin/sh

# States Count Prefix

files=
i=1

while [ $2 -ge $i ]
do
	f=$3_$1_$i.auto
	echo Creating $f...
	bin/randomnpvpa -b -e 0.85 -s $1 > $f
	files="$files $f"
	i=$(expr $i + 1)
done

