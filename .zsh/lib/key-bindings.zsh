bindkey -e  ## emacs key bindings

# historical backward/forward search with linehead string binded to ^P/^N
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^p" history-beginning-search-backward-end
bindkey "^n" history-beginning-search-forward-end
#bindkey "\\ep" history-beginning-search-backward-end
#bindkey "\\en" history-beginning-search-forward-end

function copy-line-as-kill() {
  zle kill-line
  print -rn $CUTBUFFER | pbcoly
}

function paste-as-yank() {
  pbpaste
}

zle -N paste-as-yank
bindkey "^y" paste-as-yank

