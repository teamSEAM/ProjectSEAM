#!/bin/sh

# remove trailing whitespace and convert tabs to spaces
sed 's/[ \t]*$//' | ./preprocessor | ./seam
