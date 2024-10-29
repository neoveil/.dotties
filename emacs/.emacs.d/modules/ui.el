(require 'packages)

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

(use-package page-break-lines
  :diminish
  :commands (global-page-break-lines-mode)
  :config (global-page-break-lines-mode 1))

(use-package dashboard
  :commands (dashboard-setup-startup-hook)
  :hook (dashboard-mode . (lambda () (setq-local mode-line-format nil)))
  :bind ("C-c d g" . dashboard-refresh-buffer)
  :config
  (setq-default
   dashboard-startup-banner 'logo
   dashboard-center-content t
   dashboard-vertically-center-content t
   dashboard-projects-backend 'projectile
   dashboard-icon-type 'all-the-icons
   dashboard-set-file-icons t
   dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-init-info
                               dashboard-insert-items
                               dashboard-insert-newline
                               dashboard-insert-footer)
   dashboard-items '((recents   . 10) (projects  . 10))
   initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-setup-startup-hook))

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(add-to-list 'default-frame-alist '(alpha-background . 97))
(assq-delete-all 'continuation fringe-indicator-alist)

(provide 'ui)
