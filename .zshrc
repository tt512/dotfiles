#         _
#   _____| |_  _ _ __
# _|_ (_-< ' \| '_/ _|
#(_)__/__/_||_|_| \__|
#
# Zplugin {{{
if [[ ! -a $HOME/.zplugin ]]; then
  git clone https://github.com/zdharma/zplugin.git $HOME/.zplugin
fi
source $HOME/.zplugin/bin/zplugin.zsh
autoload -Uz _zplugin
(( ${+_comps} )) && _comps[zplugin]=_zplugin

zplugin light zsh-users/zsh-history-substring-search
zplugin light zsh-users/zsh-completions
zplugin light zsh-users/zsh-autosuggestions
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
export GOPATH=$HOME/go

# deduplicate path values
typeset -U PATH path
PATH=$PATH:$HOME/bin
PATH=$PATH:$GOPATH/bin
export PATH=$PATH

if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
fi
# }}}
# Prompt {{{
function _zsh_user_prompt() {
  local t="%F{magenta}%T%f"
  local dir="%F{yellow}%~%f"
  local ret="[%(?.%F{green}.%F{red})%?%f]"
  local p="%B%F{cyan}%#%f%b"
  echo "$t $dir $ret\n$p "
}
PROMPT=$(_zsh_user_prompt)

setopt transient_rprompt

autoload -Uz vcs_info
autoload -Uz add-zsh-hook
autoload -Uz colors
zstyle ':vcs_info:*' max-exports 3
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:git:*' formats '[%F{cyan}%b%f%c%u]' '%m'
zstyle ':vcs_info:git:*' actionformats '[%F{cyan}%b%f%c%u]' '%m' '<!%a>'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{green}*%f"
zstyle ':vcs_info:git:*' unstagedstr "%F{yellow}*%f"

function _update_vcs_info_msg() {
  local -a messages
  local prompt
  LANG=en_US.UTF-8 vcs_info
  # Print nothing if get no vcs_info
  if [[ -z ${vcs_info_msg_0_} ]]; then
    prompt=""
  else
    [[ -n "$vcs_info_msg_0_" ]] && messages+=( "${vcs_info_msg_0_}" )
    [[ -n "$vcs_info_msg_1_" ]] && messages+=( "%F{yellow}${vcs_info_msg_1_}%f" )
    [[ -n "$vcs_info_msg_2_" ]] && messages+=( "%F{red}${vcs_info_msg_2_}%f" )
    # join with space
    prompt="${(j: :)messages}"
  fi
  RPROMPT="$prompt"
}
add-zsh-hook precmd _update_vcs_info_msg
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
