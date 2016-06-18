local expectedTemplateDir=${SHELL_GOODIES_ROOT/$HOME/\~}/lib/git_template
local configuredTemplateDir=$(git config init.templateDir)

if [[ "$expectedTemplateDir" != "$configuredTemplateDir" ]]; then
    if [[ -z "$configuredTemplateDir" ]]; then
        echo "You have non Git template dir configured."
    else
        echo "The Goodie git template dir is: $expectedTemplateDir"
        echo "You have configured: $configuredTemplateDir"
    fi
    echo
    echo "Consider executing:"
    echo "   git config --global init.templatedir $SHELL_GOODIES_ROOT/git_template"
fi
if zstyle -m ':shell-goodies:dvcs:git' clonedir '*'; then
    function _git_clone {
        local url="$1"
        local clonedir
        zstyle -s ':shell-goodies:dvcs:git' clonedir clonedir
        cd "$clonedir"
        git clone $url
        cd ${url:t:r}
        [[ -x .git/hooks/ctags.sh ]] && .git/hooks/ctags.sh
    }
    alias -s git=_git_clone
fi

if [[ -n "$BREW_PREFIX" ]] && [[ "$BREW_PREFIX/bin/zsh" = "$SHELL" ]]; then
    # Use git completion from system zsh.
    local git_completion=$(echo /usr/share/zsh/[[:digit:]].*/functions/_git(N[-1]))
    if [[ -f "$git_completion" ]] ; then
        ln -sf "$git_completion" "$HOME/.goodies/fpath/_dir"
    fi
fi
