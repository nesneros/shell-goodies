function dm-vbox {
    local machineName=$1
    docker-machine create -d virtualbox $machineName
    dm-select $machineName

    dm-updateEtcHosts
}

function dm-select {
    local name=$1
    if [[ -z "$name" ]] ; then
        print "No machine name specified"
        [[ -z "$DOCKER_MACHINE_NAME" ]] && return 1
        print "Re-selecting '$DOCKER_MACHINE_NAME'"
        name=$DOCKER_MACHINE_NAME
    fi
    shinit=$(docker-machine env $name 2>&1)
    rc=$?
    if [[ $rc = 0 ]] ; then
        eval $shinit
    else
        if echo $shinit | grep -q " is not running" ; then
            docker-machine start $name
            eval $(docker-machine env $name)
            rc=$?
        else
            echo "Error: $shinit"
        fi
    fi
    docker-machine ssh "$name" sudo udhcpc > /dev/null
    return $rc
}

_dm_complete() {
    declare -a machines_cmd
    machines_cmd=($(docker-machine ls|tail -n +2|awk '{print $1":"$3"("$4")"}'))
    _describe 'machines' machines_cmd
}

compdef _dm_complete dm-select

function dkr {
    extraargs=""
    if [ "$1" = '-X' ] ; then
        shift
        extraargs=(-e DISPLAY=$(ifconfig vboxnet0 |grep inet | cut -d' ' -f2):0)
        socat TCP-LISTEN:6000,reuseaddr,fork UNIX-CLIENT:"/private/tmp/com.apple.launchd.3nhqv0gCYZ/org.macosforge.xquartz:0" &
        open -a XQuartz
    fi
    
    first=$1
    shift
    case "$first" in
        rmall)
            command docker rm "$@" $(docker ps -qa)
            ;;
        @*)
            local machine=${first[2,-1]}
            local swarmOpt
            if [ ${machine[1,1]} = '@' ] ; then
                swarmOpt='--swarm'
                machine=${machine[2,-1]}
            fi
            (
                # Is there really no simpler way to get the errror
                local e="$(docker-machine env $swarmOpt $machine || echo "ERROR")"
                [ "$e" = 'ERROR' ] && exit 1
                eval "$e"
                echo "${swarm/--/}@ $DOCKER_HOST"
                docker $extraargs "$@"
            )
            ;;
        *)
            command docker "$first" $extraargs "$@"
            ;;
    esac
}

function _newEtcHosts {
    local tmpFile="$1"
    if ! command diff -q "$tmpFile" /etc/hosts > /dev/null ; then
        echo "Updating /etc/hosts"
        sudo mv /etc/hosts /etc/hosts.bak
        sudo chmod +r "$tmpFile"
        sudo cp "$tmpFile" /etc/hosts
        return 0
    else
        return 1
    fi    
}

function dm-updateEtcHosts {
    local tmpFile=$(mktemp ${TMPDIR:-/tmp}/dm-hosts.XXXXXXXXXX)
    local suffix='machine'
    # Generate new /etc/hosts by cat the old one without .machine entries, and generate entries for all machines
    cat <(grep -v ".${suffix}$" /etc/hosts) <(docker-machine ls | sed  -n "s/\([^ ]*\).*tcp:\/\/\(.*\):.*/\2 \1.${suffix}/p" | sort) >! $tmpFile
    if _newEtcHosts $tmpFile ; then
        grep ".${suffix}$" /etc/hosts
    fi
    command rm $tmpFile
}

alias dm=docker-machine
compdef dm=docker-machine 2>/dev/null || true

compdef dkr=docker 2>/dev/null || true

function m {
    local cmd=$1
    shift
    docker-machine "$cmd" $DOCKER_MACHINE_NAME "$@"
}

# gloud - this must be done before k8s. kubectl can be install via gcloud
if [[ -d $GOODIES_GCLOUD_INSTALL_DIR ]]; then
    if [ -f $GOODIES_GCLOUD_INSTALL_DIR/path.zsh.inc ]; then
        source '/home/jan/opt/google-cloud-sdk/path.zsh.inc'
    fi
    if [ -f $GOODIES_GCLOUD_INSTALL_DIR/completion.zsh.inc ]; then
        source '/home/jan/opt/google-cloud-sdk/completion.zsh.inc'
    fi
fi

# Kubernetes
if type kubectl > /dev/null ; then
    source <(kubectl completion zsh)
    alias kc=kubectl
    compdef kc=kubectl 2>/dev/null || true
fi

# Minikube
if type minikube > /dev/null ; then
    function use-minikube {
        local shinit=$(minikube docker-env 2>&1)
        if echo $shinit | grep -q "Host is not running" ; then
            minikube start
            eval $(minikube docker-env)
            rc=$?
        else
            eval $shinit
        fi
        export DOCKER_MACHINE_NAME='*kube*'

        if type kubectl > /dev/null ; then
            kubectl config use-context minikube
        fi
        local ip=$(minikube ip)
        echo "Dashboard: http://$ip:30000/"
        return $rc 
    }
fi
