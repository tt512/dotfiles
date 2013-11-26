# Language setting
export LANG=ja_JP.UTF-8

# Root directory
export ZSH=$HOME/.zsh

# PATH for texlive
export PATH=/usr/local/texlive/2013/bin/x86_64-linux:$PATH

# Load all of the config files in ~/oh-my-zsh that end in .zsh
for config_file ($ZSH/lib/*.zsh); do
  source $config_file
done

