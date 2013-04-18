#!/bin/sh
echo "compiling"

gcc -fPIC NeticaEx.c -c -I. -L../bin 
cp -f NeticaEx.o ../lib
