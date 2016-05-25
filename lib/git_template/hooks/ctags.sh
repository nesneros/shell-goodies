#! /usr/bin/env bash
set -e

# Function to call ctags/etags
gentags() {
    local exuberant # Set to t if we have exuberant ctags
    local file="$1"
    local root="$2"
    local cmd=etags
    # Preference is to exuberant ctags, then ctags, and finally etags
    if type ctags-exuberant ; then 
        cmd=ctags-exuberant
        exuberant=t
    elif type ctags >/dev/null ; then
        cmd=ctags
        # Check is ctags is exuberant
        ctags --version | head -n 1 | grep --silent --ignore-case excuberant && exuberant=t
    fi
    local a
    if [[ "$exuberant" = t ]] ; then
        a=(-e --tag-relative -L - -f"$file" --languages=-Vera,REXX)
    else
        a=(-o "$file" -)
    fi
    git ls-files | grep "^$root" | "$cmd" "${a[@]}"
}

scriptDir=$(dirname "$0")
topLevelDir=$(git rev-parse --show-toplevel)
gitDir="$(git -C "$scriptDir" rev-parse --git-dir 2>/dev/null)"
# scriptDir is within git dir. In submodule repo the above command works, in the root git repo it returns the empty string. Strange!?! (git version 2.7.2)
# In a root repo the git dir is .git, so take the parent as topLevelDir
[[ -z "$topLevelDir" ]] && topLevelDir="$gitDir/.."

tmpTagsFile="$topLevelDir/.tags.tmp$$"
trap 'rm -f "$tmpTagsFile"' EXIT
outputFile="$gitDir/ctags.out"

date -u > "$outputFile"

if [[ -f "$gitDir/projectile" ]] ; then
    root=$(cat "$gitDir/projectile")
    if [[ ! -d "$root" ]] ; then
        echo "ERROR: Projectile root '$root' does not exist" | tee -a "$outputFile" > /dev/stderr
        exit 99
    fi
    echo "Projectile root: $root" >> "$outputFile"
fi
cd "$topLevelDir"
gentags "$tmpTagsFile" "$root">> "$outputFile" 2>&1
mv "$tmpTagsFile" "$topLevelDir/.tags"
