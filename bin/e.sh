#! /bin/bash

while [[ "$1" = -* ]] ; do
    case "$1" in
        (--git)
            d=$(realpath "${2:-.}")
            cd "$d"
            root="$(git rev-parse --show-toplevel 2> /dev/null)"
            [[ -z "$root" ]] && echo "No git repo in $d" && exit 5
            cmd=(--eval "(magit-status \"$root\")")
            ;;
        (--)
            shift
            break
            ;;
    esac
    shift
done

[[ -z "$cmd" ]] && cmd=$*

if ! type emacsclient > /dev/null ; then
    emacs "$@"
else
    x=$(emacsclient --alternate-editor '' --eval '(x-display-list)' 2>/dev/null)
    ([[ -z "$x" ]] || [[ "$x" = 'nil' ]]) && extras=--create-frame
    emacsclient $extras --no-wait "${cmd[@]}" >/dev/null
fi
