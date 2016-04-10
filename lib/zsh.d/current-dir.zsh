currentDirFile=$HOME/.goodies/current_dir

_store_pwd() {
    pwd >| $currentDirFile
}

if [[ -f "$currentDirFile" ]]; then
    local dir="$(<$currentDirFile)"
    [[ -d "$dir" ]] && cd "$dir"
fi

chpwd_functions=( ${chpwd_functions[@]} _store_pwd )

