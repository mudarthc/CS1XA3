#!/bin/bash

clear
echo '               .__                                ._.
__  _  __ ____ |  |   ____  ____   _____   ____   | |
\ \/ \/ // __ \|  | _/ ___\/  _ \ /     \_/ __ \  | |
 \     /\  ___/|  |_\  \__(  <_> )  Y Y  \  ___/   \|
  \/\_/  \___  >____/\___  >____/|__|_|  /\___  >  __
             \/          \/            \/     \/   \/'

echo "What is your name?"
FS=":" read -p "enter your name " \VAR1 
            
echo "Hi $VAR1, you are currently here: $(pwd)" 

#git status
#part1 

#part2
#git diff >>  changes.log

#part3
#grep "#TODO" . -R  >> todo.log

#part4 
#ghc -fno-code *.hs &>> error.log 
