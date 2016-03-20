alias e=e.sh
alias egit='e.sh --git'
alias se='e.sh --su'

export EDITOR=e.sh
export VISUAL=$EDITOR    

# Print the file of the current Emacs buffer
efile() {
    emacsclient -e '(with-current-buffer (window-buffer (selected-window)) buffer-file-name)' 2>/dev/null | read d
    [[ "${pipestatus[1]}" -ne 0 || $d = 'nil' ]] && echo "Failed to get directory from emacs" >&2 && return 1
    echo ${d:Q}
}

edir() {
    echo "${$(efile):h}"
}

cde() {
    cd "$(edir)"
}
