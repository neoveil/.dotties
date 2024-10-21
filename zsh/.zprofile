# terminal
export TERMINAL=kitty

# clipmenu launcher
export CM_LAUNCHER=rofi

# disable webkit gpu rendering (because xwidget render blank on emacs)
export WEBKIT_DISABLE_COMPOSITING_MODE=1

# editor
export EDITOR="emacsclient -c"
export VISUAL="emacsclient -c"

# browser
export BROWSER="google-chrome-stable"

# xdg stuff
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

# give me that good old private bin
if [ -d "$HOME/.local/bin" ]; then
  export PATH="$HOME/.local/bin:$PATH"
fi

# gtk
export GTK_THEME="Arc-Dark"
export GTK_ICON_THEME="Adwaita"
export GDK_DPI_SCALE="1.25"

# dotfiles
export DOTFILES="$HOME/.dotties"

# ssh
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"

# gpg
export GPG_TTY=$(tty)

# rust
. "$HOME/.cargo/env"

# ghcup
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# xorg
if [ -z "$DISPLAY" ] && [ "$XDG_VTNR" = 1 ]; then
  exec startx
fi
