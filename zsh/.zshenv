# -*- mode: sh; -*-

# editor
export EDITOR="emacsclient -r"
export VISUAL="emacsclient -r"

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

# dotfiles
export DOTFILES="$HOME/.dotties"

# github
export GLAMOUR_STYLE="dracula"

# ssh
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"

# gpg
if [[ -n "$TTY" ]]; then
  export GPG_TTY=$(tty)
fi

# rust
. "$HOME/.cargo/env"

# ghcup
[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"

# ruby
export GEM_HOME="$(gem env user_gemhome)"
export PATH="$PATH:$GEM_HOME/bin"

# go
export PATH="$PATH:$(go env GOPATH)/bin"

# java
export JAVA_HOME="/usr/lib/jvm/default"

# libvirt
export LIBVIRT_DEFAULT_URI="qemu:///system"

# fzf
export FZF_DEFAULT_OPTS="\
--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 \
--color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 \
--color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 \
--color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4 \
-m \
--bind=ctrl-u:preview-page-up \
--bind=ctrl-d:preview-page-down \
--bind=ctrl-k:preview-up \
--bind=ctrl-j:preview-down"

# eza
export EXA_COLORS="\
uu=36:\
gu=37:\
sn=32:\
sb=32:\
da=34:\
ur=34:\
uw=35:\
ux=36:\
ue=36:\
gr=34:\
gw=35:\
gx=36:\
tr=34:\
tw=35:\
tx=36:"
