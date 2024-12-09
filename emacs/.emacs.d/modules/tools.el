;; -*- lexical-binding: t; -*-

(require 'packages)

(use-package browse-kill-ring
  :bind
  ("C-c b y" . browse-kill-ring))

(use-package crux
  :bind
  (("C-<return>" . crux-smart-open-line)
   ("C-S-<return>" . crux-smart-open-line-above)
   ("C-," . crux-duplicate-current-line-or-region)
   ("C-c s e" . crux-sudo-edit)))

(use-package pinentry
  :init
  (setq-default epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-feature xwidget
  :hook
  (xwidget-webkit-mode . (lambda ()
			   (display-line-numbers-mode -1)
			   (setq header-line-format nil))))

(use-feature abbrev
  :diminish)

(use-feature autorevert
  :diminish 'auto-revert-mode)

(use-feature dired-x
  :config
  (setq-default dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(defun wdired--register-restore-line-hl-after-edit-advice ()
  (mapc
   (lambda (fn) (advice-add fn :after (lambda (&rest _) (hl-line-mode))))
   '(wdired-finish-edit wdired-abort-changes)))

(use-feature dired
  :hook
  ((dired-mode . hl-line-mode)
   (dired-mode . auto-revert-mode)
   (wdired-mode . (lambda () (hl-line-mode -1))))
  :bind
  (:map dired-mode-map
        (("." . dired-up-directory)
         ("C-." . dired-omit-mode)
         ("q" . (lambda () (interactive) (quit-window t)))))
  :config
  (setq-default
   dired-dwim-target t
   dired-listing-switches "-alh --group-directories-first"
   dired-free-space 'separate
   dired-kill-when-opening-new-dired-buffer t)
  (wdired--register-restore-line-hl-after-edit-advice))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-feature uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package which-key
  :diminish
  :config
  (which-key-mode 1))

(use-package smex
  :bind
  (("M-x" . smex)
   ("C-c C-c M-x" . execute-extended-command))
  :config
  (setq-default smex-save-file (file-name-concat user-emacs-directory ".smex-items")))

(use-feature ido
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido-completing-read+
  :after ido
  :config
  (ido-ubiquitous-mode 1))

(use-package flx-ido
  :after ido-completing-read+
  :config
  (setq-default
   ido-enable-flex-matching t
   flx-ido-use-faces nil)
  (flx-ido-mode 1))

(use-package magit
  :bind
  (("C-c m s" . magit-status)
   ("C-c m l" . magit-log)
   ("C-c m d" . magit-diff)
   ("C-c m i" . magit-init)
   ("C-c m c c" . magit-clone)
   ("C-c m c s" . magit-clone-shallow)
   ("C-c m r a" . magit-remote-add))
  :config
  (setq-default git-commit-summary-max-length 70))

(use-package move-text
  :bind
  (("C-M-<up>" . move-text-up)
   ("C-M-<down>" . move-text-down)))

(provide 'tools)
