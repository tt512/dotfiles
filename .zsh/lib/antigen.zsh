source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
#antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle command-not-found
antigen bundle rbenv
antigen bundle nvm
antigen bundle tmux

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-completions --loc=src

# Load the theme.
#antigen theme robbyrussell

# Tell antigen that you're done.
antigen apply
