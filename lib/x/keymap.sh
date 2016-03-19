#! /bin/sh
sleep 2
set -e

# Remap keys.
# Escape: Caps Lock
# Caps Lock: Control if pressed with other key, if only key tapped it is escape

if type xcape >/dev/null ; then
    setxkbmap -option 'caps:swapescape'
    spare_modifier=Hyper_L
    xmodmap -e "keycode 66 = $spare_modifier"
    xmodmap -e "remove mod4 = $spare_modifier"
    xmodmap -e "add Control = $spare_modifier"
    xmodmap -e "keycode 255 = Escape"
    xcape -e "$spare_modifier=Escape"
fi

