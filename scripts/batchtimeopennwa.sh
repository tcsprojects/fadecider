#!/bin/sh

n=0.00
a=0.00

for i in `ls *.opennwa`
do
  echo $i
  m=`cat $i | grep "real" | sed 's/real //'`
  n=`echo $n + 1.0 | bc`
  a=`echo $a + $m | bc`
done
echo $a / $n | bc -l
