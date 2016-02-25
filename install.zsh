#! /usr/bin/env zsh -e

local dir="${0:h:A}"
local goodiesDir="$HOME/.goodies"

mkdir -p "$goodiesDir"
local prog
local neededProgs=(emacs emacsclient git)
case "$OSTYPE" in
    (darwin*)
        neededProgs=(${neededProgs[@]} brew)
        ;;
esac

for prog in $neededProgs;  do
    if ! type "$prog" > /dev/null; then
        echo "ERROR: $prog is not installed"
        exit 1
    fi
done

local force createInitFile symlink
while [[ "$1" = -* ]]; do
    case $1 in
        (--create-init-file) 
            createInitFile=t
            ;;
        (--force)
            force=t
            ;;
        (*)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
    shift
done

createInitFile() {
    local initFile="$goodiesDir/init"
    if [[ -f "$initFile" ]] && [[ "$force" != 't' ]]; then
        initFile="$initFile.template"
    fi
    cp "$dir/lib/init.template" "$initFile"
}

doInstall() {
    git config --global init.templatedir "$dir/git_template"
    mkdir -p "$goodiesDir/fpath"
    ln -sf "$(brew --prefix)/Library/Contributions/brew_zsh_completion.zsh" "$goodiesDir/fpath/_brew"
    local names=(zshrc zshenv zprofile zlogin)
    local existingFiles
    
    for n in $names; do
        local f="$HOME/.${n}"
        if [[ -e "$HOME/.${n}" ]]; then
            echo "File $f exits"
            exitingFiles=t
        fi
    done
    if [[ "$exitingFiles" = t ]] && [[ "$force" != t ]]; then
        echo "Use --force to overwrite existing files"
    else
        for n in $names; do
            local f="$HOME/.${n}"
            ln -sf "$dir/$n.zsh" "$f"
        done
    fi
}

if [[ "$1" = 'doIt' ]]; then
    doInstall
else
    cat <<EOF
The command will install the Shell Goodies. More precise
it will symlink zsh startup files from $HOME to Shell Goodies files.

Type '${0} doIt' to perform the installation.
EOF
fi
