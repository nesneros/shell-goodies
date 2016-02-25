_iterm2_buffer_words() {
    local expl
    local -a w
    w=( ${(u)=$(iterm2-buffer-contents | tail -n 100 | grep -o -E '[a-zA-Z0-9_.@~\:/-]+' | sort -u | grep -E ".{4}")} )
    for e in $w; do
        #   echo $e
    done
    _wanted values expl 'words from current iterm2 buffer' compadd -a w
}

zle -C iterm2-buffer-words-prefix   complete-word _generic
zle -C iterm2-buffer-words-anywhere complete-word _generic
#bindkey '^X^X' iterm2-buffer-words-prefix
bindkey '^X^X' iterm2-buffer-words-anywhere
zstyle ':completion:iterm2-buffer-words-(prefix|anywhere):*' completer _iterm2_buffer_words
zstyle ':completion:iterm2-buffer-words-(prefix|anywhere):*' ignore-line current
zstyle ':completion:iterm2-buffer-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'

