#!/bin/sh

~/Temp/wali-opennwa-4.0rc1/bin/nwa-complement.exe -o $1.temp $1
~/Temp/wali-opennwa-4.0rc1/bin/nwa-is-empty.exe $1.temp
rm $1.temp

