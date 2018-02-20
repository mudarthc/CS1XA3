#!/bin/bash

clear 

echo "local repo"

git status 

echo "uncommited changes in file"

rm changes.log 

git diff >> changes.log
