for p in apt systemctl; do
    if type "$p" >/dev/null ; then
        alias "$p"="sudo $p"
    fi
done
