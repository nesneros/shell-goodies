export CHPWD_PROFILE

# see http://superuser.com/questions/521657/zsh-automatically-set-environment-variables-for-a-directory
_chpwd_profiles() {
    local profile context

    context=":chpwd:profiles:$PWD"
    zstyle -s "$context" profile profile || profile='default'

    if (( ${+parameters[CHPWD_PROFILE]} == 0 )); then
        typeset -g CHPWD_PROFILE
        local CHPWD_PROFILES_INIT=1
        (( ${+functions[chpwd_profiles_init]} )) && chpwd_profiles_init
    elif [[ $profile != $CHPWD_PROFILE ]]; then
        (( ${+functions[chpwd_leave_profile_$CHPWD_PROFILE]} )) \
            && chpwd_leave_profile_${CHPWD_PROFILE}
    fi
    
    if [[ $profile != $CHPWD_PROFILE ]] && (( ${+functions[chpwd_profile_$profile]} )) ; then
        if [[ profile != 'default' ]] ; then
            local dir
            dir=$PWD
            while zstyle -s ":chpwd:profiles:$dir" profile p && [[ $profile == $p ]] ; do
                CHPWD_PROFILE_ROOT_DIR=$dir
                dir=$(dirname $dir)
            done
        fi
        print "Switching to profile: $profile (${CHPWD_PROFILE_ROOT_DIR/$HOME/\~})"
        chpwd_profile_${profile}
    fi  

    CHPWD_PROFILE="${profile}"
    return 0
}

_chpwd_profiles_enter() {
    print "Switching to profile: $profile (${CHPWD_PROFILE_ROOT_DIR/$HOME/\~})"
}

_chpwd_profiles_leave() {
    
}

# Add the chpwd_profiles() function to the list called by chpwd()!
chpwd_functions=( ${chpwd_functions[@]} _chpwd_profiles )

