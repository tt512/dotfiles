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
alias ll='ls -al'
alias la='ls -AC'
alias sl=ls # often screw this up

#alias grep="grep --color=always"

#alias emacs 'emacsclient --alternate-editor="" -n'

alias g++='g++ -fdiagnostics-color=auto'

if [[ "$OSTYPE" == "darwin"* ]]; then
  #alias vim='/Applications/MacVim.app/Contents/MacOS/Vim "$@"'
  #alias gvim='/Applications/MacVim.app/Contents/MacOS/mvim "$@"'
else
  gvim () { command gvim --remote-silent "$@" || command gvim "$@"; }
fi

# colordiff
if [[ -x `which colordiff` ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi
