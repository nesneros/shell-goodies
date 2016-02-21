export SHELL_GOODIES_ROOT=${0:h:A}
PATH=$SHELL_GOODIES_ROOT/bin:$PATH

[[ -d $HOME/bin ]] && PATH=$HOME/bin:$PATH

if [[ -z "$JAVA_HOME" ]] && [[ -x /usr/libexec/java_home ]] ; then
    export JAVA_HOME=$(/usr/libexec/java_home)
fi

if type brew >/dev/null; then
    export BREW_PREFIX=$(brew --prefix)
    if [[ -d "$BREW_PREFIX/opt/coreutils" ]]; then
        PATH="$BREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH"
        #MANPATH="$BREW_PREFIX/opt/coreutils/libexec/gnuman:$MANPATH"
    fi
fi
typeset -U path
#typeset -U manpath
