export PAGER="less"
export LESS="-g -i -M -R -F -X"
export EDITOR=nvim

typeset -U path PATH
path=(~/.yarn/bin $path)
export PATH
