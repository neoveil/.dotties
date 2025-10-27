# -*- mode: sh; -*-
# vim:ft=sh

# terminal
export TERMINAL=kitty

# uwsm
if uwsm check may-start; then
  exec uwsm start hyprland.desktop
fi
