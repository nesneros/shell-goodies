setopt autopushd

setopt share_history # between shells
setopt inc_append_history # conserve time order


# Inspired by Zoppo
# Create an alias for a command with some options.
# Either create new alias or add options to existing alias
alias+() {
    alias "$1"="${aliases[$1]:-$1} $argv[2,-1]"
}

## grep
export GREP_COLOR='37;45'           # BSD.
export GREP_COLORS="mt=$GREP_COLOR" # GNU.
alias+ grep --color=auto --directories=skip # Why would you ever want grep to treat dirs like normal files?

## ls
if type dircolors >/dev/null; then
    eval "$(dircolors --sh)"
    alias+ ls --color=auto --group-directories-first --human-readable
fi
alias ll='ls -l'
alias la='ls -lA'

alias -g L='|less'
alias -g H='|head'
alias -g T='|tail -100f'
alias -g G='|grep -e'
alias -g S='|sort'

alias md='mkdir -p'
compdef md=mkdir
alias h='history -f 1 | less +G'
alias top='sudo htop'

alias+ du -h
alias+ df -h

alias -s .gz=z
alias -s .zip=z
alias -s .Z=z

alias openports="lsof -n -P -i4TCP|grep LISTEN"

# Reload the shell (i.e. invoke as a login shell)
alias reload="exec $SHELL -l"

### History settings

HISTFILE="$HOME/.goodies/zhistory"
HISTSIZE=5000     # The maximum number of entries to save in the internal history.
SAVEHIST=5000     # The maximum number of entries to save in the history file.

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_BEEP                 # Beep when accessing non-existent history.

### Directory navigation

setopt AUTO_CD              # Auto changes to a directory without typing cd.
setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.
setopt PUSHD_TO_HOME        # Push to home directory when no argument is given.
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt AUTO_NAME_DIRS       # Auto add variable-stored paths to ~ list.
setopt MULTIOS              # Write to multiple descriptors.

### Misc options

setopt EXTENDED_GLOB        # Use extended globbing syntax.
unsetopt CLOBBER            # Do not overwrite existing files with > and >>.

### Disable glob for some commands
for e in find locate mdfind rsync scp; do
    alias "$e=noglob ${aliases[$e]:-$e}"
done

### Disable correction for some commands
for e in ag cp grep ln mv; do
    alias "$e=nocorrect ${aliases[$e]:-$e}"
done

#alias d='dirs -v'

#for index ({1..9}) alias "$index"="cd +${index}"; unset index

# Lists the ten most used commands.
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"

autoload -U zmv

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

# urlencode text
function urlencode {
    print "${${(j: :)@}//(#b)(?)/%$[[##16]##${match[1]}]}"
}

