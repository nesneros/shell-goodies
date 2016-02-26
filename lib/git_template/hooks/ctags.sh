#! /usr/bin/env bash
set -e

scriptDir=$(dirname "$0")
topLevelDir=$(git -C "$scriptDir" rev-parse --show-toplevel)
gitDir="$(git -C "$scriptDir" rev-parse --git-dir)"
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
git ls-files | grep "^$root" | ctags -e --tag-relative -L - -f"$tmpTagsFile" --languages=-Vera,REXX >> "$outputFile" 2>&1
mv "$tmpTagsFile" "$topLevelDir/.tags"

