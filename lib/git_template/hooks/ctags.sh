#!/bin/bash -e

gitDir="$(git rev-parse --git-dir)"
d="$gitDir/.."
trap 'rm -f "$d/$$.tags"' EXIT
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
git ls-files | grep "^$root" | ctags -e --tag-relative -L - -f"$d/tmp$$.tags" --languages=-Vera,REXX >> "$outputFile" 2>&1
mv "$d/tmp$$.tags" "$d/.tags"

