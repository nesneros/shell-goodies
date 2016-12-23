if [[ -s $HOME/.rvm/scripts/rvm ]]; then
    source "$HOME/.rvm/scripts/rvm"
    export PATH="$PATH:$HOME/.rvm/bin"

    if [[ -e $HOME/.rvm/bin/rvm-prompt ]]; then
        export RPROMPT="\$($HOME/.rvm/bin/rvm-prompt)"
    fi

fi
