;; -*- lexical-binding: t; -*-

(require 'packages)

(use-packages
 svg-lib)

(use-theme dracula
           dracula-use-24-bit-colors-on-256-colors-terms t)

(use-feature faces
  :config
  (set-face-attribute 'default nil :family "JetBrainsMono NF" :height 150)
  (set-face-attribute 'line-number nil :slant 'normal)
  (set-face-attribute 'line-number-current-line nil :foreground "#bd93f9")
  (set-face-background 'show-paren-match "#41589c"))

(use-package all-the-icons
  :commands (all-the-icons-install-fonts)
  :config
  (if (file-exists-p (file-name-concat
                      (or (getenv "XDG_DATA_HOME")
                          (file-name-concat (getenv "HOME") ".local" "share"))
                      "fonts"
                      all-the-icons-fonts-subdirectory
                      "all-the-icons.ttf"))
      (message "`all-the-icons' fonts already installed, skipping download/install process")
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :diminish
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package anzu
  :after diminish
  :diminish
  :commands (global-anzu-mode)
  :config
  (set-face-attribute 'anzu-mode-line nil :foreground "#bd93f9" :weight 'bold)
  (set-face-attribute 'anzu-mode-line-no-match nil :foreground "#ff6666" :weight 'bold)
  (global-anzu-mode 1))

(use-package dired-rainbow
  :config
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
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
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

;; looks nice, but i'm still thinking about it
;; (use-package powerline
;;   :commands (powerline-default-theme)
;;   :config
;;   (setq-default
;;    powerline-display-hud nil
;;    powerline-default-separator 'bar)
;;   (powerline-default-theme))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(add-to-list 'default-frame-alist '(alpha-background . 97))
(assq-delete-all 'continuation fringe-indicator-alist)

(provide 'ui)
