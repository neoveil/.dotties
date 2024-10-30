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
