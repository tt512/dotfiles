# Root directory
export ZSH=$HOME/.zsh

# load zgen
#source "${HOME}/.zgen/zgen.zsh"

# check if there's no init script
#if ! zgen saved; then
#    echo "Creating a zgen save"
#    # plugins
#    zgen load zsh-users/zsh-history-substring-search
#    #zgen load $HOME/.zsh/lib/histsubst-config.zsh
#    #zgen load /path/to/super-secret-private-plugin
#
#    # completions
#    zgen load zsh-users/zsh-completions src
#
#    # theme
#    #zgen oh-my-zsh themes/arrow
#
#    # save all to init script
#    zgen save
#fi

# Load all of the config files in ~/oh-my-zsh that end in .zsh
for config_file ($ZSH/lib/*.zsh); do
  source $config_file
done
