#!/bin/sh

WALLPAPER_DIR="$HOME/Pictures/.backgrounds/"
CURRENT_WALLPAPER=$(hyprctl hyprpaper listloaded)
WALLPAPER=$(find "$WALLPAPER_DIR" -type f ! -name "$(basename "$CURRENT_WALLPAPER")" | shuf -n 1)

ln -sf $WALLPAPER $WALLPAPER_DIR/current
hyprctl hyprpaper reload ,$WALLPAPER
pidof hyprlock > /dev/null && kill -USR2 $(pidof hyprlock)
