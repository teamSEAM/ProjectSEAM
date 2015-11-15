#! /bin/sh

COMPILER=seam

./preprocessor < $1 | ./$COMPILER
