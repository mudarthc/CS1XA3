#!/bin/bash

clear 

echo "local repo"

git status 

echo "uncommited changes in file"

git status > changes.log
