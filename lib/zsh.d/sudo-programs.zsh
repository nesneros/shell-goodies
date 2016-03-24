for p in apt aptitude systemctl; do
    if type "$p" >/dev/null ; then
        alias "$p"="sudo $p"
    fi
done
