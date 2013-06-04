#!/bin/sh

# Directory Command Extension

for i in `ls $1/*.open`
do
  echo $i
  time -p $2 $i 2> $i.$3
done


