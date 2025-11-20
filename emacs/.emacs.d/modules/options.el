;; -*- lexical-binding: t; -*-

;; user info settings
(setq-default
 user-full-name "Lucas A. F. Vitor"
 user-mail-address "luca.oetdbem@gmail.com")

;; basic settings
(setq-default
 isearch-wrap-pause 'no-ding
 enable-recursive-minibuffers t
 ring-bell-function 'ignore
 confirm-kill-emacs 'y-or-n-p
 native-comp-async-report-warnings-errors nil
 apropos-do-all t
 read-process-output-max (* 4 1024 1024))

;; files settings
(setq-default
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 require-final-newline t
 fill-column 120
 vc-follow-symlinks t
 confirm-nonexistent-file-or-buffer nil
 load-prefer-newer t)

;; clipboard settings
(setq-default
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

(eval-when-compile
  (require 'functions)
  (require 'packages))

(use-feature windmove
  :config
  (windmove-default-keybindings 'meta))

(use-feature savehist
  :config
  (savehist-mode 1))

(use-feature saveplace
  :config
  (save-place-mode 1))

(use-feature recentf
  :config
  (recentf-mode 1))

(use-feature abbrev
  :diminish)

(use-feature subword
  :diminish)

;; misc
(fset 'yes-or-no-p 'y-or-n-p)

(put-all
 upcase-region   disabled nil
 downcase-region disabled nil)

(provide 'options)
