#!/bin/bash

#PS1="\w\\$ "
export PS1='\[\033[32m\]\h:\w\$\[\033[0m\] '
alias cls="clear"
alias ls="ls --color=tty -FBU"

# 以前の HISTORY にマッチする行は追加しない。
export HISTCONTROL=ignoreboth:erasedups

# "history"，"exit"，1文字と2文字のコマンドは保存しない
export HISTIGNORE=history:exit:?:??

# カーソルキーで検索
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

# Ctrl-S の端末ロックを無効
stty stop undef

# historyの共有
export PROMPT_COMMAND="history -a"

# **でパスを展開
shopt -s globstar

# cdを省略
shopt -s autocd

# update terminal window size
shopt -s checkwinsize

# ディレクトリでないものは変数の名前とみなす
shopt -s cdable_vars
export hogehoge=""

# ワイルドカードの拡張
shopt -s extglob

# historyに時刻を記録
export HISTTIMEFORMAT='%F %T '

# settings for peco
_replace_by_history() {
#    local l=$(HISTTIMEFORMAT= history | tac | sed -e 's/^\s*[0-9]*\+\s\+//' | awk '!a[$0]++' | peco --query "$READLINE_LINE")
    local l=$(HISTTIMEFORMAT= history | tac | sed -e 's/^\s*[:0-9 \-]*//' | awk '!a[$0]++' | peco --query "$READLINE_LINE")

    READLINE_LINE="$l"
    READLINE_POINT=${#l}
}
#bind -x '"\C-r": _replace_by_history'
#bind    '"\C-xr": reverse-search-history'
bind -x    '"\C-xr": _replace_by_history'
