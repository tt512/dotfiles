#!/bin/bash

# list of files that will not make link
EXCEPTIONS=(.git README.md setup.sh)
# list of files that located current directory
FILES=(`ls -A`)

# remove elements that are in exception list
FILES=($(comm -23 <(printf "%s\n" "${FILES[@]}") <(printf "%s\n" "${EXCEPTIONS[@]}")))

for FILE in ${FILES[@]}
do
  ln -s $PWD/$FILE $HOME/$FILE
done

