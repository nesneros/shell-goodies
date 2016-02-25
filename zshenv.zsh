local logFile="$HOME/.goodies/log.txt"
: >| "$logFile"

typeset -F SECONDS

_goodies_log() {
    echo "$SECONDS: $1" >> "$logFile"
}

_goodies_log "BEGIN: zshenv.zsh"

_source() {
    local start=$SECONDS
    source "$1"
    integer duration=$((1000 * ($SECONDS-start)))
    [[ $duration -gt 0 ]] && _goodies_log "$duration ms to source '$1'"
}

_prezto_source() {
    local preztoFile="$HOME/.zprezto/runcoms/$1"
    [[ -e "$preztoFile" ]] && _source "$preztoFile"
}

export SHELL_GOODIES_ROOT=${${(%):-%N}:A:h} # $0 doesn't work because it is a starup file for zsh
_goodies_log "SHELL_GOODIES_ROOT=$SHELL_GOODIES_ROOT"

path=("$HOME/bin"(N) "$SHELL_GOODIES_ROOT/bin" $path /usr/local/{bin,sbin}(N))

# Make sure BREW_PREFIX is defined before prezto. Prezto sources ~/.zprofiles which used BREW_PREFIX
if [[ "$OSTYPE" = "darwin"* ]]; then
    export BREW_PREFIX=$(brew --prefix)

    # if zsh is install with Homebrew, warn if system zsh is shell
    local homebrewZsh="$BREW_PREFIX/bin/zsh"
    if [[ -f "$homebrewZsh" ]] && [[ "$SHELL" == "/bin/zsh" ]] ; then
        echo "Warning: Shell is /bin/zsh but zsh is also install in '$homebrewZsh'"
    fi

    if [[ -z "$JAVA_HOME" ]] && [[ -x /usr/libexec/java_home ]]; then
        export JAVA_HOME=$(/usr/libexec/java_home)
    fi
fi

_prezto_source zshenv

typeset -U path
#typeset -U manpath

_goodies_log "END: zshenv.zsh"
