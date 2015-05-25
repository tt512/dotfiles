## smart urls
#autoload -U url-quote-magic
#zle -N self-insert url-quote-magic

## file rename magick
#bindkey "^[m" copy-prev-shell-word

## jobs
#setopt long_list_jobs

## pager
export PAGER="less"
export LESS="-R"

#export LC_CTYPE=$LANG

#export SHELL=/usr/bin/zsh
export EDITOR=vim

export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color

#export LANG=en_US.UTF-8
#export LC_CTYPE=en_US.UTF-8
#printf "\33]701;$LC_CTYPE\007"
#export LC_COLLATE=en_US.UTF-8
#export LC_TIME=en_US.UTF-8
#export LC_NUMERIC=en_US.UTF-8
#export LC_MONETARY=en_US.UTF-8
#export LC_MESSAGES=en_US.UTF-8
#export MM_CHARSET=en_US.UTF-8


