#!/bin/sh

# Directory Command SubYes SubNo

for i in `ls $1`
do
  echo Checking $1/$i
  cat $1/$i | $2 > result.temp
  if [ "$?" = "0" ]; then
    if grep -qF "Automaton is universal" result.temp; then
      mv $1/$i $3/$i
    else
      mv $1/$i $4/$i
    fi
  fi
  rm result.temp
done


