#!/bin/bash

DOT_FILES=(.zshrc .bashrc .vimrc .tmux.conf .keysnail.js .Xkbmap .emacs)

for file in ${DOT_FILES[@]}
do
  ln -s $HOME/dotfiles/$file $HOME/$file
done

