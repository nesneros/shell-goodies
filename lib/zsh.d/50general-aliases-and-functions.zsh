setopt autopushd

setopt share_history # between shells
setopt inc_append_history # conserve time order

# Why would you ever want grep to treat dirs like normal files?
alias grep="${aliases[grep]:-grep} --directories=skip"

alias -g L='|less'
alias -g H='|head'
alias -g T='|tail -100f'
alias -g G='|grep -e'
alias -g S='|sort'

alias md='mkdir -p'
compdef md=mkdir
alias h='history -f 1 | less +G'
alias top='sudo htop'

if type emacs > /dev/null ; then
    alias e=e.sh
    alias egit='e.sh --git'
    export EDITOR=e.sh
fi
export VISUAL=$EDITOR    

alias -s tar.gz=z

mcd() {
    mkdir -p "$1" && cd "$1"
}

function fwhere {
    whence -a -v $1 | perl -n -e'/is a shell function from (.*)/ && print "$1\n"'
}

function fedit {
    e $(fwhere $1)
}

# cd to a file
cd() {
    if (( $# != 1 )); then
        builtin cd "$@"
    elif [[ -f "$1" ]]; then
        builtin cd "$1:h"
    else
        builtin cd "$1"
    fi
}

# get public ip
function myip {
    local api
    case "$1" in
	"-4")
	    api="http://v4.ipv6-test.com/api/myip.php"
	    ;;
	"-6")
	    api="http://v6.ipv6-test.com/api/myip.php"
	    ;;
	*)
	    api="http://ipv6-test.com/api/myip.php"
	    ;;
    esac
    curl -s "$api"
    echo
}

# from http://chneukirchen.org/blog/archive/2013/03/10-fresh-zsh-tricks-you-may-not-know.html
imv() {
    local src dst
    for src; do
        [[ -e $src ]] || { print -u2 "$src does not exist"; continue }
        dst=$src
        vared dst
        [[ $src != $dst ]] && mkdir -p $dst:h && mv -n $src $dst
    done
}

