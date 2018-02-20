#!/bin/bash

clear 

echo "local repo"

git status 

echo "uncommited changes in file"

git diff  > changes.log
