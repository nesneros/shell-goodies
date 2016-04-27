currentDirFile=$HOME/.goodies/current_dir

_store_pwd() {
    pwd >| $currentDirFile
}

# INSIDE_NAUTILUS_PYTHON is defined when opening terminal from Nautilus. In that
# case don't change dir
if [[ -z "${INSIDE_NAUTILUS_PYTHON+defined}" && -f "$currentDirFile" ]]; then
    local dir="$(<$currentDirFile)"
    [[ -d "$dir" ]] && cd "$dir"
fi

chpwd_functions=( ${chpwd_functions[@]} _store_pwd )
