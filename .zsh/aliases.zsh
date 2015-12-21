# List direcory contents
case "${OSTYPE}" in
freebsd*|darwin*)
  alias ls="ls -G -w"
  ;;
linux*)
  alias ls="ls --color"
  ;;
esac
#alias lsa='ls -lah'
alias ll='ls -l'
alias lla='ls -la'
alias la='ls -AC'
alias sl=ls # often screw this up

setopt auto_pushd
setopt pushd_ignore_dups
setopt pushd_minus

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

#alias grep="grep --color=always"

alias emacs='emacsclient --alternate-editor="" -n'
alias p='pygmentize -O style=monokai -f console256'

#alias g++='g++ -fdiagnostics-color=auto'

if [[ "$OSTYPE" == "darwin"* ]]; then
  mvim () { command mvim --remote-silent "$@" || command mvim "$@"; }
  alias gvim='mvim'
else
  gvim () { command gvim --remote-silent "$@" || command gvim "$@"; }
fi

# colordiff
if [[ -x `which colordiff` ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi
