if [ "$(pamixer --get-mute)" = "false" ]; then
    pamixer -m
else
    pamixer -u
fi
