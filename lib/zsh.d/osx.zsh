function pfd() {
    /usr/bin/osascript <<EOT
        tell application "Path Finder"
          set front_path to the POSIX path of the target of the front finder window
          return front_path
        end tell
EOT
}

battery() {
    local maxcap curcap extconnected fully_charged
    ioreg -c AppleSmartBattery -w0 | \
	grep -o '"[^"]*" = [^ ]*' | \
	sed -e 's/= //g' -e 's/"//g' | \
	sort | \
	while read key value; do
	    case $key in
		"MaxCapacity")
		    maxcap=$value;;
		"CurrentCapacity")
		    curcap=$value;;
		"ExternalConnected")
		    extconnect=$value;;
                "FullyCharged")
                    fully_charged=$value;;
	    esac
	    if [[ -n $maxcap && -n $curcap && -n $extconnect ]]; then
		if [[ "$curcap" == "$maxcap" || "$fully_charged" == "Yes" && $extconnect == "Yes"  ]]; then
		    return
		fi
		local charge=$(( 100.0 * $curcap / $maxcap ))
		printf "%3.1f\n" $charge
		break
	    fi
	done
}
