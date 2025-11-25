;;; ui.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(require 'functions)

(setq-default
 inhibit-startup-screen t
 blink-cursor-blinks 0
 fringe-indicator-alist (assq-delete-all 'continuation fringe-indicator-alist)
 default-frame-alist (cons '(alpha-background . 90) default-frame-alist))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-org-config))

(use-feature faces
  :config
  (set-face-attribute 'default nil :family "JetBrainsMono NF" :height 150 :weight 'semibold))

(use-package all-the-icons
  :config
  (all-the-icons--install-fonts-if-not-installed))

(use-package all-the-icons-dired
  :diminish
  :defer t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package dired-rainbow
  :config
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dimmer
  :config
  (dimmer-configure-magit)
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-feature menu-bar
  :config
  (menu-bar-mode 0))

(use-feature tool-bar
  :config
  (tool-bar-mode 0))

(use-feature scroll-bar
  :config
  (scroll-bar-mode 0))

(use-feature simple
  :config
  (column-number-mode 1))

(use-feature display-line-numbers
  :config
  (global-display-line-numbers-mode 1))

(use-feature pixel-scroll
  :config
  (pixel-scroll-precision-mode 1))

(use-feature windmove
  :bind
  (("C-c <left>"  . windmove-swap-states-left)
   ("C-c <right>" . windmove-swap-states-right)
   ("C-c <up>"    . windmove-swap-states-up)
   ("C-c <down>"  . windmove-swap-states-down))
  :config
  (windmove-default-keybindings 'meta))

(use-feature window
  :bind
  ("C-x C-o" . other-window))

(provide 'ui)
