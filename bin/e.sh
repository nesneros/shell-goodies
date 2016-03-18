#! /usr/bin/env bash
set -e

declare -a cmd

while [[ "$1" = -* ]] ; do
    case "$1" in
        (--git)
            # Open magit status on the specified path, or if none on the cwd.
            git=t
            ;;
        (-n|--new-frame)
            hasFrame=nil # Indicate no frame, so a new is created
            ;;
        (-x|--kill)
            # Kill the server (if it exist).
            # If more arguments specified start a new server and client.
            emacsclient --eval "(kill-emacs)" 2>/dev/null
            noDefaultArg=t
            ;;
        (-t|--tty)
            # Create client in the terminal,
            tty=t
            ;;
        (-U|--update-packages)
            # Upgrade installed packages
            emacsclient --alternate-editor '' --eval "(package-utils-upgrade-all)"
            noDefaultArg=t
            ;;
        (--)
            shift
            break
            ;;
    esac
    shift
done

if [[ "$git" = t ]]; then
    d=$(realpath "${2:-.}")
    shift || true
    cd "$d"
    root="$(git rev-parse --show-toplevel 2> /dev/null)"
    [[ -z "$root" ]] && echo "No git repo in $d" && exit 5
    cmd=(--eval "(magit-status \"$root\")")
fi

cmd=("${cmd[@]}" "$@")

if [[ ${#@} -eq 0 ]]; then
    [[ "$noDefaultArg" = t ]] && exit 0
    [[ "${#cmd}" -eq 0 ]] && cmd=(.)
fi

if [[ "$tty" = t ]]; then
    cmd=(--tty ${cmd[@]})
else
    case "$OSTYPE" in
        (darwin*)   frame="'ns" ;;
        (linux-gnu) frame="'x" ;;
        (*)         frame="'unknown" ;;
    esac
    if [[ -z "$hasFrame" ]]; then
        hasFrame=$(emacsclient --eval "(member $frame (mapcar 'framep (frame-list)))" 2>/dev/null) || :
    fi
    [[ -z "$hasFrame" || "$hasFrame" = 'nil' ]] && cmd=(--create-frame ${cmd[@]})
    cmd=(--no-wait "${cmd[@]}")
fi

emacsclient --alternate-editor '' "${cmd[@]}" &>/dev/null
