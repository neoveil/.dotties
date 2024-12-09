;; -*- lexical-binding: t; -*-

;; basic settings
(setq-default
 inhibit-startup-screen t
 blink-cursor-blinks 0
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
 require-final-newline t
 load-prefer-newer t
 tramp-auto-save-directory "/tmp")

;; clipboard settings
(setq-default
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

;; misc
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings 'meta)
(pixel-scroll-precision-mode 1)
(savehist-mode 1)

(provide 'options)
