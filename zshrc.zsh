_goodies_log "BEGIN: zshrc.zsh"

fpath=("$HOME/.goodies/fpath" "${fpath[@]}")

_prezto_source zshrc

autoload -Uz compinit
compinit

goodiesInitFile=$HOME/.goodies/init
[[ -f $goodiesInitFile ]] && _source "$goodiesInitFile"
unset goodiesInitFile

osTag=unknown
case $OSTYPE in
    (darwin*)
        osTag=darwin
        ;;
esac

zstyle -s ':shell-goodies:init' dir d
for f in ${SHELL_GOODIES_ROOT}/lib/zsh.d/{,$osTag/}*.zsh(N) ${d:-/somethingThatDoesNotExist}/*.zsh(N); do
    _source $f
done 
unset d osTag

typeset -U path
typeset -U manpath

_goodies_log "END: zshrc.zsh"
