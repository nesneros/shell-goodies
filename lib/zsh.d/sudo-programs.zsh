for p in apt aptitude snap systemctl; do
    if type "$p" >/dev/null ; then
        alias "$p"="sudo $p"
    fi
done
