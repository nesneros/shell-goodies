_goodies_log "BEGIN: zprofile.zsh"

_prezto_source zprofile

if [[ -n "$BREW_PREFIX" ]]; then
    # Add coreutils to path if installed
    if [[ -d "$BREW_PREFIX/opt/coreutils" ]]; then
        local d="$BREW_PREFIX/opt/coreutils/libexec/gnubin"
        path=("$d" ${path:#${d}})
        #MANPATH="$BREW_PREFIX/opt/coreutils/libexec/gnuman:$MANPATH"
    fi
fi

_goodies_log "END: zprofile.zsh"
