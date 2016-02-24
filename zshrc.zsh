_goodies_log "BEGIN: zshrc.zsh"

fpath=("$HOME/.goodies/fpath" "${fpath[@]}")

_prezto_source zshrc

autoload -Uz compinit
compinit

for f in ${SHELL_GOODIES_ROOT}/lib/zsh.d/*.zsh(N) $HOME/etc/zsh.d/*.zsh(N); do
    _source $f
done 

typeset -U path
typeset -U manpath

_goodies_log "END: zshrc.zsh"
