(require 'packages)

(use-packages
 dash
 sudo-edit)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package pinentry
  :ensure t
  :config
  (setq-default epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-package dired
  :hook
  (dired-mode . hl-line-mode)
  (dired-mode . auto-revert-mode)
  (wdired-mode . (lambda () (hl-line-mode -1)))
  :bind
  (:map dired-mode-map
        ("C-." . dired-up-directory)
        ("." . dired-up-directory))
  :config
  (setq-default
   dired-dwim-target t
   dired-listing-switches "-alh --group-directories-first"
   dired-free-space 'separate)
  (mapc
   (lambda (fn) (advice-add fn :after (lambda (&rest _) (hl-line-mode))))
   '(wdired-finish-edit wdired-abort-changes)))

(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)
   ("C-c C-c M-x" . execute-extended-command))
  :config (setq-default smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package ido-completing-read+
  :ensure t
  :config
  (setq-default ido-auto-merge-work-directories-length nil)
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package magit
  :ensure t
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m d" . magit-diff)
   ("C-c m i" . magit-init)
   ("C-c m c c" . magit-clone)
   ("C-c m c s" . magit-clone-shallow))
  :config (setq-default git-commit-summary-max-length 70))

(use-package move-text
  :ensure t
  :bind
  (("C-M-<up>" . move-text-up)
   ("C-M-<down>" . move-text-down)))

(provide 'tools)
