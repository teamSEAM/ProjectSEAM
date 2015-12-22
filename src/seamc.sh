#!/bin/sh

SOURCE=$1
GEN_C=${SOURCE%.*}.c

# sed: remove trailing whitespace
sed -e 's/[ \t]*$//' -e '/^$/d' $SOURCE | ./preprocessor | ./seam #> $GEN_C

rm $GEN_C
