# Language setting
#export LANG=ja_JP.UTF-8

# Root directory
export ZSH=$HOME/.zsh

# Load all of the config files in ~/oh-my-zsh that end in .zsh
for config_file ($ZSH/lib/*.zsh); do
  source $config_file
done

export PATH=$PATH:`ruby -e 'puts Gem.user_dir'`/bin
