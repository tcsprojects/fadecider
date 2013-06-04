#!/bin/sh

for i in `ls *.auto`
do
  cat $i | $1 > $i.open
done

