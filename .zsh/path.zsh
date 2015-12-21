PATH=$PATH:$(ruby -rubygems -e "puts Gem.user_dir")/bin
PATH=$PATH:$HOME/bin
export PATH=$PATH
eval "$(direnv hook zsh)"
