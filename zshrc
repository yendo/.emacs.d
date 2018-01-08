# -*-shell-script-*-

# colored man

function man (){
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        LANG=C \
        man "$@"
}


# 4.0.4-33

export SHELL=/usr/bin/zsh      # by csh2zsh 0.42
export G_BROKEN_FILENAMES=1
umask 066

setopt AUTO_PUSHD
alias gd='dirs -v; echo -n "select number: "; read newdir; cd +"$newdir"'

# 補完キャッシュ

zstyle ':completion:*' use-cache true

# グローバルエイリアス

alias -g L="| less"
alias -g G='| grep'
#alias -g C='| cat -n'
alias -g W='| wc'
alias -g H='| head'
alias -g T='| tail'

# シェル変数の設定

complete=enhance

#set filec
#set nobeep
#set autocorrect
#set autolist
#set dunique
#unset autologout
#unset verbose
#set color
#history=1000
#savehist=1000
#set correct = all
#histdup=erase

eval `dircolors ~/.colorrc`
export ZLS_COLORS=$LS_COLORS
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

###
# Set shell options
###
setopt auto_menu auto_cd correct auto_name_dirs auto_remove_slash
setopt extended_history hist_ignore_dups hist_ignore_space prompt_subst
setopt pushd_ignore_dups rm_star_silent sun_keyboard_hack
setopt extended_glob list_types no_beep always_last_prompt
setopt cdable_vars sh_word_split auto_param_keys
setopt print_eight_bit

# 履歴

HISTFILE=$HOME/.zsh-history           # 履歴をファイルに保存する
HISTSIZE=1000000                       # メモリ内の履歴の数
SAVEHIST=1000000                       # 保存される履歴の数
setopt extended_history               # 履歴ファイルに時刻を記録
setopt share_history
function history-all { history -i 1 } # 全履歴の一覧を出力する

setopt hist_reduce_blanks     # コマンドラインの余計なスペースを排除。
setopt hist_expire_dups_first # ヒストリに重複があった場合、古い方を削除する。
setopt hist_find_no_dups      # 一度発見された履歴は二度と表示しない。
setopt histignorealldups

# 補完

autoload -U compinit
compinit

fignore=('.o' '\~' '.out' '.aux' '.pyc')
path=( $path /home/yendo/bin )
symlinks=expand
matchbeep=nomatch
#dspkanji=euc

#bindkey '^U' backward-kill-line
#bindkey '^R' i-search-back

#bindkey  "\e[A" history-search-backward
#bindkey  "\e[B" history-search-forward

bindkey "^[OA" history-beginning-search-backward
bindkey "^[OB" history-beginning-search-forward

bindkey ";5C" forward-word
bindkey ";5D" backward-word

export WORDCHARS='*?[]~=&;!#$%^(){}<>'


#stty -istrip
#bindkey -m # Meta キーを有効に (日本語ダメ?)

export WORDCHARS='*?_.[]~=&;!#$%^(){}<>'

#zle -N backward-kill-word bash-backward-kill-word
bindkey "^W" backward-kill-word
#autoload -U colors
#colors


# 環境変数

export CONCURRENCY_LEVEL=6
export DISTCC_HOSTS="localhost master"

#export GREP_OPTIONS='--color=auto'

#export PILOTPORT=/dev/ttyS0
#export PILOTPORT=/dev/pilot
#export PILOTRATE=57600
#export PILOTFROM="Yoshizumi Endo <y-endo@ceres.dti.ne.jp>"
#export POPKEEP=delete
#export SENDMAIL=$HOME/bin/pilot-sendmail
#export PILOTDISPOSE=file
#export TOPILOT_MHDIR=$HOME/Mail/pilot

#export RLPR_PRINTHOST=master
export CANNAHOST=master
export COLORFGBG="white;black"
 
export CVSROOT=~/.CVS/
export EMAIL=yendo0206@gmail.com
#export NOMHNPROC=nil
export XMODIFIERS='@im=kinput2'
export AWKPATH=~/bin/awk/
export PAGER=lv
export EDITOR=jed
export VISUAL=jed
export JLESSCHARSET=japanese-euc
export LANG=ja_JP.UTF-8
export TEXINPUTS=.:~/local/tex/prosper:
#export NNTPSERVER=newsall.dti.ne.jp
#export TIMID_DIR=/etc/
export CVS_RSH=ssh

#エイリアスの設定

# cd() { builtin cd "$@"; echo $PWD; }

#namazu() { command namazu "$@" ~/Documents/.namazu/ ~/Maildir/namazu/}
#bnamazu() { command bnamazu -b  w3m  "$@" ~/text/.namazu/ ~/Maildir/namazu/}
#bnamazu-mail() { command bnamazu -b w3m  "$@" ~/Maildir/namazu/}
#bnamazu-text() { command bnamazu -b w3m  "$@" ~/text/.namazu/}

#es () {command grep -A 1 "^$@" ~/master/esp.txt | nkf -e }

apt-get () {command sudo apt-get "$@"}
aptitude () {command sudo aptitude "$@"}
getsource () {command apt-get source "$@"}

platex () {command platex "$@" 2| nkf -u}
tex2pdf () {command platex $1 |nkf -u ; dvipdfmx -f my $1:r.dvi ; acroread $1:r.pdf}


dgrep () {command env COLUMNS=${COLUMNS:-80} dpkg -l | grep "$@"}

#alias youtube='mplayer -fs -xineramascreen 1 -monitoraspect 4:3'
alias pgrep='pgrep -lf'
alias e='gnuclient'

#alias onscripter='onscripter --enable-wheeldown-advance -f /usr/share/fonts/truetype/hiragino/hiramarugo4.otf'
alias dpkg='COLUMNS=${COLUMNS:-80} dpkg'
#alias jnethack="screen -t jnethack -T jfbterm /usr/games/jnethack"
alias jnethack="screen -X encoding euc; jnethack"
alias emacs="env XMODIFIERS=@im=none emacs"
alias df="df -h"
alias gv="ggv"
alias xdvi="xdvi-ja"
alias doff="xset dpms force off"
alias du="du -h"
alias cls="clear"
alias a2ps="a2psj -p -a4"
alias total='jgawk -f total.awk'
alias whois="whois -h whois.nic.ad.jp"
#alias less="jless -iM"
alias less="lv"
alias traceroute="/usr/sbin/traceroute"
alias bar="jgawk -f bar.awk"
alias tarc="tar cvzf $1.tar.gz $1"
alias tart="tar tvzf"
alias tarx="tar xvzf"
alias dic="gnudoit -q '(online-dictionary)'"
alias bookview="/usr/bin/bookview -fk '-ms-gothic-medium-r-normal--12-*-jisx0208.1983-0'  -fn '-adobe-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*'"
alias snes9x="snes9x -joydev1 /dev/input/js0 -joymap1 1 0 3 4 6 7 5 2"
alias gnus="emacs -f gnus"

alias mymirror='mirror ~/.mirrorrc'
alias ls="ls --color=tty -FBX"
#alias w3m="screen -t w3m canfep w3m"
#alias w3m="screen -t w3m w3m"
#alias top="screen top"
#alias emacs="screen emacs"
alias shutdown="sudo /sbin/shutdown"

#if [ $uid = 0 ]; then
#    alias emacs='emacs -nw'
#else
#    alias shutdown="sudo /sbin/shutdown"
#fi


# 警告音
#if ($tty != "ttyS1") then
# alias beepcmd '/usr/bin/esdplay  /usr/local/sounds/ro_std.wav >& /dev/null'
#endif


#ウインドウタイトルとプロンプト

#setopt prompt_subst 
#prompt="%{[32m%}%m:%~%#%{[m%} "


if [ $HOST = "master" ]; then 
    PS1=$'%{\e[1;34m%}%m:%~%#%{\e[0m%} '
    alias mplayer="mplayer -vsync -slang ja -framedrop -fs -nolirc -vo dfbmga:noinput:bes"
else
    PS1=$'%{\e[5;32m%}%m:%~%#%{\e[0m%} '
fi

if [ $UID = 0 ]; then
    PS1=$'%{\e[1;32m%}%m:%~%#%{\e[0m%} '
fi
 

# set terminal title including current directory
#
case "${TERM}" in 
kterm*|xterm)
    precmd() {
        echo -ne "\033]0;${HOST%%.*}:${PWD}\007"
    }
    ;;
esac 

# エスケープシーケンス %{ ^[[32m  %} %{ ^[[m  %}


#if [ $?DISPLAY ]; then 
##   set prompt="%{\033]0;%m: %~\007%}%B%m:%~%%%b "
#    prompt="%{\033]0;%m: %~\007%}%{\e[32m%m:%~%%\e[00m%} "
#fi

#if [ $TERM = "screen" ]; then 
#    prompt="%{\e[32m%m:%~%%\e[00m%} "
#fi

# for 256 colors

if [ "$TERM" = "xterm" ]; then
    TERM="xterm-256color"
fi

if [ "$TERM" = "linux" ]; then
    LANG="en"
fi



if [ "$TERM" = "xterm" -o "$TERM" = "xterm-256color" ]
then
  hostname=`hostname -s`
  function _setcaption() { echo -ne  "\e]1;${hostname}\a\e]2;${hostname}$1\a" > /dev/tty }

  # ディレクトリを移動したら、ウィンドウのタイトルを
  # ホスト名:現在地 のように変更
  function chpwd() {  print -Pn "\e]2;%m : %~\a" }
  # 初期設定してあげる(cd . でも可)
  chpwd

  # 特定のコマンド実行中は、タイトルを ホスト名 (コマンド名) のように
  # 変更
  function _cmdcaption() { _setcaption " [$1]"; "$@"; chpwd }

#  for cmd in telnet slogin ssh rlogin rsh su sudo screen top w3m
#  do
#    alias $cmd="_cmdcaption $cmd"
#  done
fi

compctl -/g '*.rm *.ram' realplay
compctl -/g '*.dvi' xdvi-ja dvips dvipdfmx
compctl -/g '*.wmv *.mpg *.avi' mplayer
compctl -/g '*.tex' + -f platex latex tex tex2pdf
compctl -/g '*.ps' ps2pdf13
compctl -/g '*.txt' a2psj


# function peco-z-search
# {
#   which peco z > /dev/null
#   if [ $? -ne 0 ]; then
#     echo "Please install peco and z"
#     return 1
#   fi
#   local res=$(z | sort -rn | cut -c 12- | peco)
#   if [ -n "$res" ]; then
#     BUFFER+="cd $res"
#     zle accept-line
#   else
#     return 1
#   fi
# }
# zle -N peco-z-search
# bindkey '^f' peco-z-search
# source ~/.zsh.d/z.sh
# 
# function peco-select-history() {
#     # historyを番号なし、逆順、最初から表示。
#     # 順番を保持して重複を削除。
#     # カーソルの左側の文字列をクエリにしてpecoを起動
#     # \nを改行に変換
#     BUFFER="$(history -nr 1 | awk '!a[$0]++' | peco --query "$LBUFFER" | sed 's/\\n/\n/')"
#     CURSOR=$#BUFFER             # カーソルを文末に移動
#     zle -R -c                   # refresh
# }
# 
# zle -N peco-select-history
# bindkey '^R' peco-select-history


# cdr, add-zsh-hook を有効にする
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
 
# cdr の設定
zstyle ':completion:*' recent-dirs-insert both
zstyle ':chpwd:*' recent-dirs-max 500
zstyle ':chpwd:*' recent-dirs-default true
zstyle ':chpwd:*' recent-dirs-file "$HOME/.cache/shell/chpwd-recent-dirs"
zstyle ':chpwd:*' recent-dirs-pushd true

#function peco-cdr () {
#local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)
#if [ -n "$selected_dir" ]; then
#BUFFER="cd ${selected_dir}"
#zle accept-line
#fi
#zle clear-screen
#}
#zle -N peco-cdr
#bindkey '^F' peco-cdr # M-x cdに割り当て


function peco-cdr () {
local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)
if [ -n "$selected_dir" ]; then
BUFFER="cd ${selected_dir}"
zle accept-line
fi
zle clear-screen
}
zle -N peco-cdr

bindkey '^F' peco-cdr # M-x cdに割り当て
