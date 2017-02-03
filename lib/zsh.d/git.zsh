local expectedTemplateDir=${SHELL_GOODIES_ROOT}/lib/git_template
local configuredTemplateDir=$(git config init.templateDir)

if [[ "$expectedTemplateDir" != "$configuredTemplateDir" ]]; then
    if [[ -z "$configuredTemplateDir" ]]; then
        echo "You have non Git template dir configured."
    else
        echo "The Goodie git template dir is: $expectedTemplateDir"
        echo "You have configured:            $configuredTemplateDir"
    fi
    echo
    echo "Consider executing:"
    echo "   git config --global init.templatedir $expectedTemplateDir"
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
