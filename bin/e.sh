#! /usr/bin/env bash
set -e

while [[ "$1" = -* ]] ; do
    case "$1" in
        (--git)
            d=$(realpath "${2:-.}")
            cd "$d"
            root="$(git rev-parse --show-toplevel 2> /dev/null)"
            [[ -z "$root" ]] && echo "No git repo in $d" && exit 5
            cmd=(--eval "(magit-status \"$root\")")
            ;;
        (--kill)
            cmd=(--eval "(kill-emacs)")
            ;;
        (-x|--restart)
            emacsclient --alternate-editor '' --eval "(kill-emacs)"
            ;;
        (--)
            shift
            break
            ;;
    esac
    shift
done

[[ -z "$cmd" ]] && cmd=("$@")
[[ -z "$cmd" ]] && cmd=(.)

# See http://www.gnu.org/software/emacs/manual/html_mono/elisp.html#Frames for meaning of 'ns 
x=$(emacsclient --alternate-editor '' --eval "(member 'ns (mapcar 'framep (frame-list)))" 2>/dev/null)
([[ -z "$x" ]] || [[ "$x" = 'nil' ]]) && cmd=(--create-frame ${cmd[@]})
emacsclient --no-wait "${cmd[@]}" >/dev/null
