alias -s .gz=z
alias -s .zip=z
alias -s .Z=z

alias e=e.sh
alias egit='e --git'

alias openports="lsof -n -P -i4TCP|grep LISTEN"

autoload -Uz compinit
compinit

for f in ${0:h}/lib/zsh.d/*.zsh(N) $HOME/etc/zsh.d/*.zsh(N); do
    source $f
done

_imux_complete() {
    local completions array
    completions=$(ls $HOME/.tmuxinator/|grep -e '.yml$'| cut -f1 -d'.')
    array=( "${(ps:\n:)completions}" )
    compadd $array
}

compdef _imux_complete imux

source $UNUXUS_HOME/etc/profile/common

if type fasd > /dev/null 2>&1 ; then
    function fasd_type {
        # Echo output of 'type' command, and add the first line that reference a file to fasd
        \type -a "$@" > /dev/stdout > >(grep -m 1 "^$1 is /"| (read x ; fasd -A ${x#$1 is }))
    }
    alias type=fasd_type
fi

# urlencode text
function urlencode {
    print "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

