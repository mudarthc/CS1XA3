#!/bin/bash

clear
echo '
                 ___     //  ___      ___      _   __      ___       __  ___  ___          ___      __      ___         ( )  ___      ___    __  ___    /_  /    
  //  / /  / / //___) ) // //   ) ) //   ) ) // ) )  ) ) //___) )     / /   //   ) )     //   ) ) //  ) ) //   ) )     / / //___) ) //   ) )  / /        / /     
 //  / /  / / //       // //       //   / / // / /  / / //           / /   //   / /     //___/ / //      //   / /     / / //       //        / /        / /      
((__( (__/ / ((____   // ((____   ((___/ / // / /  / / ((____       / /   ((___/ /     //       //      ((___/ / ((  / / ((____   ((____    / /        / /     '

echo "What is your name?"
FS=":" read -p "enter your name " \VAR1 
            
echo "Hi $VAR1, you are currently here: $(pwd)" 

echo "Would you like to continue, $VAR1?"
FS=":" read -p "Enter yes or no " \VAR2
if [ $VAR2 = "yes" ] 
then
	 echo "what would you like to do?"
fi
#git status
#part1 

#part2
#git diff >>  changes.log

#part3
#grep "#TODO" . -R  >> todo.log

#part4 
#ghc -fno-code *.hs &>> error.log 
