;;; tools.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(require 'functions)

(use-feature files
  :bind
  (("C-c o t" . open-temp-buffer)
   ("C-c g h" . go-home)
   ("C-c f p" . find-file-at-point)
   ("C-x C-b" . buffer-menu)
   ("C-c k a" . kill-all-buffers)
   ("C-c k o" . kill-other-buffers)))

(use-feature dired
  :hook
  ((dired-mode  . hl-line-mode)
   (dired-mode  . auto-revert-mode)
   (wdired-mode . disable-hl-line-mode))
  :bind
  (("C-x C-d" . dired)
   (:map dired-mode-map
         (("."   . dired-up-directory)
          ("C-." . dired-omit-mode)
          ("q"   . quit-window-kill-buffer))))
  :config
  (setq-default
   dired-dwim-target t
   dired-listing-switches "-alh --group-directories-first"
   dired-free-space 'separate
   dired-kill-when-opening-new-dired-buffer t)
  (wdired--register-restore-hl-line-mode-on-exit))

(use-feature dired-x
  :config
  (setq-default dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

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

(use-feature term
  :hook
  (term-mode . disable-display-line-numbers-mode)
  :bind
  (("C-c s c" . shell-command)
   ("C-c s a" . async-shell-command)
   ("C-c t t" . term)
   ("C-c t o" . term-other-window))
  :config
  (put 'term 'interactive-form '(interactive (list "/usr/bin/zsh")))
  (advice-add #'term-handle-exit :around #'term--better-exit-handler-advice))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package anzu
  :diminish
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   (:map isearch-mode-map
         (([remap isearch-query-replace]        . anzu-isearch-query-replace)
          ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))))
  :config
  (global-anzu-mode 1))

(use-package ace-link
  :config
  (ace-link-setup-default))

(use-package literate-calc-mode
  :mode "\\.calc\\'"
  :bind
  ("C-c o c" . open-literate-calc-temp-buffer))

(provide 'tools)
