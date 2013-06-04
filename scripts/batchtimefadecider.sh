#!/bin/sh

n=0.00
a=0.00

for i in `ls *.fadecider`
do
#  echo $i
  m=`cat $i | grep "t = " | sed 's/.*= //' | sed 's/ sec//'`
  n=`echo $n + 1.0 | bc`
  a=`echo $a + $m | bc`
done
echo $a / $n | bc -l
