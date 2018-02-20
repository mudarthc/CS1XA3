#!/bin/bash

clear 

#part1
git status 

#part2
rm changes.log
git diff >>  changes.log

#part3
rm todo.log 

grep "#TODO" . -R  > todo.log

#part4 
#ghc -fno-code *.hs &> error.log 
