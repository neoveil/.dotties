# terminal
export TERMINAL=kitty

# clipmenu launcher
export CM_LAUNCHER=rofi

# xorg
if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
  exec startx
fi
