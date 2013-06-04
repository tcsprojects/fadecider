#!/bin/sh

# Directory Command Extension

for i in `ls $1/*.auto`
do
  echo $i
  cat $i | $2 > $i.$3
done


