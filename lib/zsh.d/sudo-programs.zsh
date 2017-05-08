function _makesudo {
    if type "$1" >/dev/null ; then
        alias "$1"="sudo $1"
    fi
}

case "$OSTYPE" in
    (linux*)
        for p in apt aptitude snap systemctl; do
            _makesudo $p
        done
        ;;
    (darvin*)
        ;;
    (*)
esac
