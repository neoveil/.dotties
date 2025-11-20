;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(use-package crux
  :bind
  (("C-<return>"   . crux-smart-open-line)
   ("C-S-<return>" . crux-smart-open-line-above)
   ("C-,"          . crux-duplicate-current-line-or-region)
   ("C-c s e"      . crux-sudo-edit)))

(use-feature dired-x
  :config
  (setq-default dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-feature dired
  :hook
  ((dired-mode . hl-line-mode)
   (dired-mode . auto-revert-mode))
  :bind
  (:map dired-mode-map
        (("."   . dired-up-directory)
         ("C-." . dired-omit-mode)
         ("q"   . (lambda () (interactive) (quit-window t)))))
  :config
  (setq-default
   dired-dwim-target t
   dired-listing-switches "-alh --group-directories-first"
   dired-free-space 'separate
   dired-kill-when-opening-new-dired-buffer t))

(use-feature wdired
  :hook
  (wdired-mode . (lambda () (hl-line-mode -1)))
  :config
  (mapc
   (lambda (f) (advice-add f :after (lambda (&rest _) (hl-line-mode))))
   '(wdired-finish-edit wdired-abort-changes)))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-feature which-key
  :diminish
  :bind
  (:map help-map
        ("C-h" . nil))
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
  (("C-c m s"   . magit-status)
   ("C-c m l"   . magit-log)
   ("C-c m d"   . magit-diff)
   ("C-c m i"   . magit-init)
   ("C-c m c c" . magit-clone-shallow)
   ("C-c m c f" . magit-clone))
  :config
  (setq-default git-commit-summary-max-length 70))

(use-package move-text
  :bind
  (("C-M-<up>"   . move-text-up)
   ("C-M-<down>" . move-text-down)))

(use-feature term
  :hook
  (term-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (put 'term 'interactive-form '(interactive (list "/usr/bin/zsh")))
  (advice-add
   'term-handle-exit
   :around
   (lambda (f &optional proc msg)
     (let ((inhibit-message t))
       (funcall f proc msg))
     (message
      "%s%s"
      proc
      (concat
       (when msg " | ")
       (string-trim (or msg ""))))
     (kill-buffer (current-buffer)))))

(provide 'tools)
