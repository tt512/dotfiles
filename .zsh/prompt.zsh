setopt prompt_subst
#PROMPT="%F{magenta}[%@]%f %F{yellow}[%~]%f %F{green}[%n@%m]%f
#%B%F{cyan}%#%f%b "
NEWLINE=$'\n'
PROMPT="%F{magenta}%T%f %F{yellow}%~%f${NEWLINE}%B%F{cyan}%#%f%b "

RPROMPT=""

autoload -Uz vcs_info
autoload -Uz add-zsh-hook
autoload -Uz is-at-least
autoload -Uz colors

# 以下の3つのメッセージをエクスポートする
#   $vcs_info_msg_0_ : 通常メッセージ用 (緑)
#   $vcs_info_msg_1_ : 警告メッセージ用 (黄色)
#   $vcs_info_msg_2_ : エラーメッセージ用 (赤)
zstyle ':vcs_info:*' max-exports 3

zstyle ':vcs_info:*' enable git svn hg bzr
## 標準のフォーマット(git 以外で使用)
## misc(%m) は通常は空文字列に置き換えられる
#zstyle ':vcs_info:*' formats '(%s)-[%b]'
#zstyle ':vcs_info:*' actionformats '(%s)-[%b]' '%m' '<!%a>'
#zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
#zstyle ':vcs_info:bzr:*' use-simple true

## Formats for git
zstyle ':vcs_info:git:*' formats '[%F{cyan}%s:%b%f%c%u]' '%m'
# Used if there is a special action, like a merge conflict
zstyle ':vcs_info:git:*' actionformats '[%F{cyan}%s:%b%f%c%u]' '%m' '<!%a>'
zstyle ':vcs_info:git:*' check-for-changes true
zstyle ':vcs_info:git:*' stagedstr "%F{green}*%f"    # %c で表示する文字列
zstyle ':vcs_info:git:*' unstagedstr "%F{yellow}*%f"  # %u で表示する文字列

## Hooks for git
#
# 今回の設定の場合はformat の時は2つ, actionformats の時は
# 3つメッセージがあるので各関数が最大3回呼び出される。
# Register functions to the hook
zstyle ':vcs_info:git+set-message:*' hooks \
  git-hook-begin \
  git-untracked \
  git-push-status \
  git-nomerge-branch \
  git-stash-count

# フックの最初の関数
# git の作業コピーのあるディレクトリのみフック関数を呼び出すようにする
# (.git ディレクトリ内にいるときは呼び出さない)
# .git ディレクトリ内では git status --porcelain などがエラーになるため
function +vi-git-hook-begin() {
  if [[ $(command git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
    # 0以外を返すとそれ以降のフック関数は呼び出されない
    return 1
  fi

  return 0
}

# untracked フィアル表示
#
# untracked ファイル(バージョン管理されていないファイル)がある場合は
# unstaged (%u) に ? を表示
# If there are untracked files,
# Add red `*' character to the unstagedstr (%u)
function +vi-git-untracked() {
  # zstyle formats, actionformats の2番目のメッセージのみ対象にする
  if [[ "$1" != "0" ]]; then
    return 0
  fi

  if command git status --porcelain 2> /dev/null \
      | awk '{print $1}' \
      | command grep -F '??' > /dev/null 2>&1 ; then

    # unstaged (%u) に追加
    hook_com[unstaged]+="%F{magenta}*%f"
  fi
}

# push していないコミットの件数表示

# リモートリポジトリに push していないコミットの件数を
# pN という形式で misc (%m) に表示する
function +vi-git-push-status() {
  # zstyle formats, actionformats の2番目のメッセージのみ対象にする
  if [[ "$1" != "1" ]]; then
    return 0
  fi

  if [[ "${hook_com[branch]}" != "master" ]]; then
    # master ブランチでない場合は何もしない
    return 0
  fi

  # push していないコミット数を取得する
  local ahead
  ahead=$(command git rev-list origin/master..master 2>/dev/null \
      | wc -l \
      | tr -d ' ')

  if [[ "$ahead" -gt 0 ]]; then
    # misc (%m) に追加
    hook_com[misc]+="(p${ahead})"
  fi
}

# マージしていない件数表示
#
# master 以外のブランチにいる場合に、
# 現在のブランチ上でまだ master にマージしていないコミットの件数を
# (mN) という形式で misc (%m) に表示
function +vi-git-nomerge-branch() {
  # zstyle formats, actionformats の2番目のメッセージのみ対象にする
  if [[ "$1" != "1" ]]; then
    return 0
  fi

  if [[ "${hook_com[branch]}" == "master" ]]; then
    # master ブランチの場合は何もしない
    return 0
  fi

  local nomerged
  nomerged=$(command git rev-list master..${hook_com[branch]} 2>/dev/null | wc -l | tr -d ' ')

  if [[ "$nomerged" -gt 0 ]] ; then
    # misc (%m) に追加
    hook_com[misc]+="(m${nomerged})"
  fi
}

# stash 件数表示
# stash している場合は :SN という形式で misc (%m) に表示
function +vi-git-stash-count() {
  # zstyle formats, actionformats の2番目のメッセージのみ対象にする
  if [[ "$1" != "1" ]]; then
    return 0
  fi

  local stash
  stash=$(command git stash list 2>/dev/null | wc -l | tr -d ' ')
  if [[ "${stash}" -gt 0 ]]; then
    # misc (%m) に追加
    hook_com[misc]+=":S${stash}"
  fi
}

function _update_vcs_info_msg() {
  local -a messages
  local prompt

  LANG=en_US.UTF-8 vcs_info

  if [[ -z ${vcs_info_msg_0_} ]]; then
    # vcs_info で何も取得していない場合はプロンプトを表示しない
    prompt=""
  else
    # vcs_info で情報を取得した場合
    # $vcs_info_msg_0_ , $vcs_info_msg_1_ , $vcs_info_msg_2_ を
    # それぞれ緑、黄色、赤で表示する
    [[ -n "$vcs_info_msg_0_" ]] && messages+=( "${vcs_info_msg_0_}" )
    [[ -n "$vcs_info_msg_1_" ]] && messages+=( "%F{yellow}${vcs_info_msg_1_}%f" )
    [[ -n "$vcs_info_msg_2_" ]] && messages+=( "%F{red}${vcs_info_msg_2_}%f" )

    # 間にスペースを入れて連結する
    prompt="${(j: :)messages}"
  fi

  RPROMPT="$prompt"
}

add-zsh-hook precmd _update_vcs_info_msg

#autoload -Uz vcs_info
#zstyle ':vcs_info:*' formats '[%b]'
#zstyle ':vcs_info:*' actionformats '[%b|%a]'
#precmd () {
#    psvar=()
#    LANG=en_US.UTF-8 vcs_info
#    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
#}
#RPROMPT="%1(v|%F{green}%1v%f|)"
#
#
## Work In Progress (wip)
## These features allow to pause a branch development and switch to another one (wip)
## When you want to go back to work, just unwip it
##
## This function return a warning if the current branch is a wip
#function work_in_progress() {
#  if $(git log -n 1 | grep -q -c wip); then
#echo "WIP!!"
#  fi
#}
##RVM and git settings
#if [[ -s ~/.rvm/scripts/rvm ]] ; then
#  RPS1='$(git_custom_status)%{$fg[red]%}[`~/.rvm/bin/rvm-prompt`]%{$reset_color%} $EPS1'
#else
#  if which rbenv &> /dev/null; then
#    RPS1='$(git_custom_status)%{$fg[red]%}[`rbenv version | sed -e "s/ (set.*$//"`]%{$reset_color%} $EPS1'
#  else
#    RPS1='$(git_custom_status) $EPS1'
#  fi
#fi
#
#setopt prompt_subst
##  PROMPT2="%B%{${fg[green]}%}#%{${reset_color}%}%b "
##  SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
##  #[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
##  PROMPT="[%{${fg[white]}%}${USER}]${PROMPT}"
##RPROMPT="[%~]"
##RPROMPT='[%D{%L:%M:%S %p}]'
##
##TMOUT=30
##TRAPALRM() { zle reset-prompt }
#
