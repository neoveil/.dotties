# init completions
autoload -U compinit && compinit

# oh my zsh path
export ZSH="$HOME/.oh-my-zsh"

# spaceship theme config
ZSH_THEME="spaceship"
SPACESHIP_USER_SHOW=always
SPACESHIP_PROMPT_ADD_NEWLINE=false
SPACESHIP_PROMPT_ORDER=(
  user
  dir
  host
  git
  exec_time
  line_sep
  jobs
  exit_code
  char
)

# plugins
plugins=(
  mvn
  gradle
  docker
  docker-compose
  extract
  httpie
  lein
  ufw
  git
  sudo
  colorize
  colored-man-pages
  zsh-autosuggestions
  zsh-syntax-highlighting
)

# zsh completions
fpath+=$ZSH_CUSTOM/plugins/zsh-completions/src

source $ZSH/oh-my-zsh.sh

# env
[ -f "$HOME/.zenv" ] && . "$HOME/.zenv"

# aliases
[ -f "$HOME/.zaliases" ] && . "$HOME/.zaliases"

# functions
[ -f "$HOME/.zfunctions" ] && . "$HOME/.zfunctions"
