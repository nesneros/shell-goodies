_goodies_log "BEGIN: zshrc.zsh"

fpath=("$HOME/.goodies/fpath" "${fpath[@]}")

_prezto_source zshrc

# Should be after prezto is loaded (done my prezto if installed)
autoload -Uz compinit
compinit -i

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

### zsh-autosuggest
_source ${SHELL_GOODIES_ROOT}/thirdparty/zsh-autosuggestions/zsh-autosuggestions.zsh
# Add history-substring-search-* widgets to list of widgets that clear the autosuggestion
ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(history-substring-search-up history-substring-search-down)

### zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
_source "${SHELL_GOODIES_ROOT}/thirdparty/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

typeset -U path
typeset -U manpath

_goodies_log "END: zshrc.zsh"
