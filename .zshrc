#         _
#   _____| |_  _ _ __
# _|_ (_-< ' \| '_/ _|
#(_)__/__/_||_|_| \__|
#
# Zinit {{{
### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust
### End of Zinit's installer chunk

zinit light zsh-users/zsh-history-substring-search
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions

zinit ice pick"async.zsh" src"pure.zsh"
zinit light sindresorhus/pure
# }}}
# Keybind {{{
bindkey -e
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down
# }}}
# History {{{
export HISTFILE=$HOME/.zsh_history
export HISTSIZE=10000
export SAVEHIST=10000
setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history
# }}}
# Alias {{{
case "${OSTYPE}" in
freebsd*|darwin*)
  alias ls="ls -G -w"
  ;;
linux*)
  alias ls="ls --color"
  ;;
esac
alias ll='ls -l'
alias lla='ls -la'
alias la='ls -AC'
alias sl=ls

function take() {
  mkdir -p $1 && cd $_
}

alias xclip='xclip -selection clipboard'
# }}}
# Envvar {{{
export PAGER="less"
export LESS="-g -i -M -R -F -X"
export EDITOR=nvim

export GOROOT="$HOME/.go"
export GOPATH="$HOME/go"

# deduplicate path values
typeset -U PATH path
export PATH="$HOME/bin:$GOPATH/bin:$PATH"
# }}}
# VTE title {{{
# Write some info to terminal title.
# This is seen when the shell prompts for input.
function _vte_title_precmd {
  print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
}
# Write command and args to terminal title.
# This is seen while the shell waits for a command to complete.
function _vte_title_preexec {
  printf "\033]0;%s\a" "$1"
}
case $TERM in
  xterm*|*rxvt*)
    add-zsh-hook precmd  _vte_title_precmd
    add-zsh-hook preexec _vte_title_preexec
    ;;
esac
## }}}
# Completion {{{
# Menu Select
zmodload zsh/complist
zstyle ':completion:*:default' menu select interactive
bindkey -M menuselect '^n' down-line-or-history
bindkey -M menuselect '^p' up-line-or-history

# Completing misc
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list _history
zstyle ':completion:*:*files' ignored-patterns '*?.o' '*?~' '*\#'
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*' use-cache true
zstyle ':completion::complete:*' cache-path $HOME/.cache/zsh

autoload -Uz compinit; compinit
# }}}

# recompile if source is newer than .zwc
# http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#Recompiling-Functions
autoload -Uz zrecompile
zrecompile -q .zshrc
zrecompile -q .zcompdump

# Profiling
#
# * loading time
#     time ( zsh -i -c exit )
# * profile zshrc loading
#     ZRCPROF=1 zsh -i -c exit
# * profile prompt printing
#     zmodload zsh/zprof; zplof | less
#
# See also .zshenv
if [[ $ZRCPROF -eq 1 ]]; then
  zprof | less
fi
