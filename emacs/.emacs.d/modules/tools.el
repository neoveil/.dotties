(require 'packages)

(use-packages
 dash
 diminish
 sudo-edit)

(use-package pinentry
  :ensure t
  :config
  (setq-default epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package xwidget
  :hook (xwidget-webkit-mode . (lambda ()
                                 (display-line-numbers-mode -1)
                                 (setq header-line-format nil))))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq-default
   projectile-switch-project-action #'projectile-dired
   projectile-project-search-path '("~/Projects/"))
  (projectile-mode 1))

(use-package dired-x
  :config
  (setq-default
   dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package dired
  :hook
  (dired-mode . hl-line-mode)
  (dired-mode . auto-revert-mode)
  (wdired-mode . (lambda () (hl-line-mode -1)))
  :bind
  (:map dired-mode-map
        ("." . dired-up-directory)
        ("C-." . dired-omit-mode)
        ("q" . (lambda () (interactive) (quit-window t))))
  :config
  (setq-default
   dired-dwim-target t
   dired-listing-switches "-alh --group-directories-first"
   dired-free-space 'separate
   dired-kill-when-opening-new-dired-buffer t)
  (mapc
   (lambda (fn) (advice-add fn :after (lambda (&rest _) (hl-line-mode))))
   '(wdired-finish-edit wdired-abort-changes)))

(use-package smex
  :ensure t
  :bind
  ("M-x" . smex)
  ("C-c C-c M-x" . execute-extended-command)
  :config (setq-default smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package ido
  :config
  (setq-default ido-auto-merge-work-directories-length nil)
  (ido-mode 1)
  (declare-function ido-everywhere "ido")
  (ido-everywhere 1))

(use-package ido-completing-read+
  :ensure t
  :config (ido-ubiquitous-mode 1))

(use-package magit
  :ensure t
  :bind
  ("C-c m s" . magit-status)
  ("C-c m l" . magit-log)
  ("C-c m d" . magit-diff)
  ("C-c m i" . magit-init)
  ("C-c m c c" . magit-clone)
  ("C-c m c s" . magit-clone-shallow)
  :config (setq-default git-commit-summary-max-length 70))

(use-package move-text
  :ensure t
  :bind
  ("C-M-<up>" . move-text-up)
  ("C-M-<down>" . move-text-down))

(provide 'tools)
