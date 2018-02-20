#!/bin/bash

clear 
git status
#part1 

#part2
git diff >>  changes.log

#part3
grep "#TODO" . -R  >> todo.log

#part4 
ghc -fno-code *.hs &> error.log 
