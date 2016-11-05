fzfDir=$SHELL_GOODIES_ROOT/thirdparty/fzf

if [[ -e $fzfDir ]] ; then
    if [[ ! "$MANPATH" == *${fzfDir}/man* && -d "$fzfDir/man" ]]; then
        export MANPATH="$MANPATH:$fzfDir/man"
    fi

    [[ $- == *i* ]] && source "$fzfDir/shell/completion.zsh" 2> /dev/null

    source "$fzfDir/shell/key-bindings.zsh"
else
    echo "FZF installation dir ($fzfDir) does not exists"
fi
