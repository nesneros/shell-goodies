local logFile="$HOME/.goodies/log.txt"
: >| "$logFile"

typeset -F SECONDS

goodies:log() {
    echo "$SECONDS: $1" >> "$logFile"
}

goodies:log "BEGIN: zshenv.zsh"

# Define TMPDIR. We want all shells to have same TMPDIR
export TMPDIR=/tmp/tmp.$USER
mkdir -p $TMPDIR
chmod 700 $TMPDIR

goodies:source() {
    local start=$SECONDS
    source "$1"
    integer duration=$((1000 * ($SECONDS-start)))
    [[ $duration -gt 0 ]] && goodies:log "$duration ms to source '$1'"
}

goodies:prezto_source() {
    local preztoFile="$HOME/.zprezto/runcoms/$1"
    [[ -e "$preztoFile" ]] && goodies:source "$preztoFile"
}

[[ -f "$HOME/.goodies/envinit" ]] && goodies:source "$HOME/.goodies/envinit"

export SHELL_GOODIES_ROOT=${${(%):-%N}:A:h} # $0 doesn't work because it is a starup file for zsh
goodies:log "SHELL_GOODIES_ROOT=$SHELL_GOODIES_ROOT"

path=("$HOME/bin/$(hostname)"(N) "$HOME/bin"(N) "$SHELL_GOODIES_ROOT/bin" $path /usr/local/{bin,sbin}(N))

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

# For Debian/Ubuntu set JAVA_HOME
if [[ -z "$JAVA_HOME" ]] && type update-alternatives >/dev/null ; then
    javaBin=$(update-alternatives --query java 2>/dev/null | grep "^Value: " | cut -f2 -d' ')
    if [[ -n "$javaBin" ]] ; then
        export JAVA_HOME=${javaBin:h:h:h}
    fi
fi

# Start gpg-agent if it is not running
if type gpg-connect-agent >/dev/null && [ -z "$GPG_TTY" ] ; then
    if [[ "$OSTYPE" == "darwin"* ]]; then
        if type gpg-agent >/dev/null && [ -z "$GPG_AGENT_INFO" ] ; then
            [ -f ~/.goodies/gpg-agent-info ] && source ~/.goodies/gpg-agent-info
            if [ -S "${GPG_AGENT_INFO%%:*}" ]; then
                export GPG_AGENT_INFO
            else
                eval $( gpg-agent --daemon --write-env-file ~/.goodies/gpg-agent-info )
            fi
        fi
    else
        gpg-connect-agent /bye
    fi
    GPG_TTY=$(tty)
    export GPG_TTY
fi

goodies:prezto_source zshenv

typeset -U path
#typeset -U manpath

goodies:log "END: zshenv.zsh"
