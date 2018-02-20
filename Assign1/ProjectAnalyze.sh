#!/bin/bash

clear 

#part1
git status 

echo "uncommited changes in file"

#part2

git diff >> changes.log

#part3
grep "#TODO" . -R  >> todo.log

#part4 

ghc -fno-code file.hs >> error.log 
