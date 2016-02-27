goodies:log "BEGIN: zshrc.zsh"

fpath=("$HOME/.goodies/fpath" "${fpath[@]}")

goodies:prezto_source zshrc

# Should be after prezto is loaded (done my prezto if installed)
autoload -Uz compinit
compinit

goodiesInitFile=$HOME/.goodies/init
[[ -f $goodiesInitFile ]] && goodies:source "$goodiesInitFile"
unset goodiesInitFile

osTag=unknown
case $OSTYPE in
    (darwin*)
        osTag=darwin
        ;;
esac

zstyle -s ':shell-goodies:init' dir d
for f in ${SHELL_GOODIES_ROOT}/lib/zsh.d/{,$osTag/}*.zsh(N) ${d:-/somethingThatDoesNotExist}/*.zsh(N); do
    goodies:source $f
done 
unset d osTag

### k 
goodies:source "${SHELL_GOODIES_ROOT}/thirdparty/k/k.sh"

### zaw
goodies:source "${SHELL_GOODIES_ROOT}/thirdparty/zaw/zaw.zsh"

### zsh-autosuggest
#goodies:source ${SHELL_GOODIES_ROOT}/thirdparty/zsh-autosuggestions/zsh-autosuggestions.zsh
## Add history-substring-search-* widgets to list of widgets that clear the autosuggestion
#ZSH_AUTOSUGGEST_CLEAR_WIDGETS+=(history-substring-search-up history-substring-search-down)

### zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)
goodies:source "${SHELL_GOODIES_ROOT}/thirdparty/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

### zsh-history-substring-search
# This must be loaded after syntax highlighting
goodies:source "${SHELL_GOODIES_ROOT}/thirdparty/zsh-history-substring-search/zsh-history-substring-search.zsh"
# bind UP and DOWN arrow keys
zmodload zsh/terminfo
bindkey "$terminfo[kcuu1]" history-substring-search-up
bindkey "$terminfo[kcud1]" history-substring-search-down

# bind UP and DOWN arrow keys (compatibility fallback
# for Ubuntu 12.04, Fedora 21, and MacOSX 10.9 users)
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down

# bind P and N for EMACS mode
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# bind k and j for VI mode
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down





typeset -U path
typeset -U manpath

goodies:log "END: zshrc.zsh"
