;; -*- lexical-binding: t; -*-

(require 'packages)

(use-packages
 browse-kill-ring
 projectile-ripgrep)

(use-package pinentry
  :commands (pinentry-start)
  :init
  (setq-default epg-pinentry-mode 'loopback)
  (pinentry-start))

(use-feature xwidget
  :hook (xwidget-webkit-mode . (lambda ()
                                 (display-line-numbers-mode -1)
                                 (setq header-line-format nil))))

(use-feature abbrev
  :diminish)

(defun projectile--ignore-projects-starting-with (prefixes)
  (lambda (root)
    (seq-some
     (lambda (prefix) (string-prefix-p prefix root t))
     prefixes)))

(use-package projectile
  :defer t
  :commands (projectile-mode projectile-dired)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (setq-default
   projectile-switch-project-action 'neotree-projectile-action
   projectile-project-search-path '("~/Projects/" "~/Projects/archive/")
   projectile-ignored-project-function (projectile--ignore-projects-starting-with
                                        '("~/.dotties/emacs/.emacs.d/straight/")))
  (projectile-mode 1))

(use-feature dired-x
  :config
  (setq-default
   dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-feature dired
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

(use-feature uniquify
  :config
  (setq-default uniquify-buffer-name-style 'forward))

(use-package neotree
  :bind
  ("C-c n t" . neotree-toggle)
  (:map neotree-mode-map
        ("." . neotree-select-up-node))
  :config
  (setq-default
   neo-vc-integration '(face)
   neo-theme 'icons
   neo-smart-open t
   neo-hide-cursor t
   neo-show-slash-for-folder nil
   neo-autorefresh t
   neo-create-file-auto-open t
   neo-show-hidden-files t
   neo-show-updir-line nil
   neo-window-width 30))

(use-feature neotree-file-info
  :after neotree
  :hook (neotree-mode . neotree-display-file-info))

(use-package which-key
  :diminish
  :commands (which-key-mode)
  :config (which-key-mode 1))

(use-package smex
  :bind
  ("M-x" . smex)
  ("C-c C-c M-x" . execute-extended-command)
  :config (setq-default smex-save-file (concat user-emacs-directory ".smex-items")))

(use-feature ido
  :commands (ido-everywhere)
  :config
  (ido-mode 1)
  (ido-everywhere 1))

(use-package ido-completing-read+
  :after ido
  :commands (ido-ubiquitous-mode)
  :config (ido-ubiquitous-mode 1))

(use-package flx-ido
  :commands (flx-ido-mode)
  :after ido-completing-read+
  :config
  (setq-default
   ido-enable-flex-matching t
   flx-ido-use-faces nil)
  (flx-ido-mode 1))

(use-package magit
  :bind
  ("C-c m s" . magit-status)
  ("C-c m l" . magit-log)
  ("C-c m d" . magit-diff)
  ("C-c m i" . magit-init)
  ("C-c m c c" . magit-clone)
  ("C-c m c s" . magit-clone-shallow)
  ("C-c m r a" . magit-remote-add)
  :config (setq-default git-commit-summary-max-length 70))

(use-package move-text
  :bind
  ("C-M-<up>" . move-text-up)
  ("C-M-<down>" . move-text-down))

(provide 'tools)
