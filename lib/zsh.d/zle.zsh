# from http://chneukirchen.org/blog/archive/2015/02/10-fancy-zsh-tricks-you-may-not-know.html
_physical_up_line()   { zle backward-char -n $COLUMNS }
_physical_down_line() { zle forward-char  -n $COLUMNS }
zle -N physical-up-line _physical_up_line
zle -N physical-down-line _physical_down_line
bindkey "\e\e[A" physical-up-line    # Alt arrow up
bindkey "\e\CP"  physical-up-line    # Escape Control P
bindkey "\e\e[B" physical-down-line  # Alt arraw down
bindkey "\e\CN" physical-down-line  # Escape Control N

# Move to where the arguments belong.
after-first-word() {
    zle beginning-of-line
    zle emacs-forward-word
    zle forward-char
}

after-second-word() {
    zle beginning-of-line
    zle emacs-forward-word
    zle emacs-forward-word
    zle forward-char
}

zle -N after-first-word
zle -N after-second-word
bindkey "^X1" after-first-word
bindkey "^X2" after-second-word

export WORDCHARS='*?_-.[]~&;!#$%^(){}<>/'
