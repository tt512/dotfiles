# Language setting
#export LANG=ja_JP.UTF-8

# Root directory
export ZSH=$HOME/.zsh

# Load all of the config files in ~/oh-my-zsh that end in .zsh
for config_file ($ZSH/lib/*.zsh); do
  source $config_file
done

export PATH=$PATH:`ruby -e 'puts Gem.user_dir'`/bin
export PATH=$PATH:$HOME/bin
export LANG=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
printf "\33]701;$LC_CTYPE\007"
export LC_COLLATE=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export MM_CHARSET=en_US.UTF-8
