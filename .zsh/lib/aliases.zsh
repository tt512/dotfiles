# List direcory contents
alias ls='ls --color=auto'
#alias lsa='ls -lah'
alias ll='ls -al'
alias la='ls -AC'
alias sl=ls # often screw this up

#alias grep="grep --color=always"

#alias emacs 'emacsclient --alternate-editor="" -n'

alias g++='g++ -fdiagnostics-color=auto'

gvim () { command gvim --remote-silent "$@" || command gvim "$@"; }
