(require 'variables)
(require 'package-init)

(add-to-list 'load-path local-directory)
(load-file custom-file-path)

(setq-default
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 require-final-newline t
 custom-file custom-file-path
 fill-column 120
 blink-cursor-blinks 0
 tab-width 2
 indent-tabs-mode nil
 isearch-wrap-pause 'no-ding
 enable-recursive-minibuffers t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t
 ring-bell-function 'ignore
 tramp-auto-save-directory "/tmp"
 vc-follow-symlinks t
 epg-pinentry-mode 'loopback
 compilation-scroll-output t
 confirm-nonexistent-file-or-buffer nil
 dracula-use-24-bit-colors-on-256-colors-terms t
 vterm-always-compile-module t
 vterm-max-scrollback 50000
 max-lisp-eval-depth 10000
 confirm-kill-emacs 'y-or-n-p)

(unless (eq system-type 'darwin) (menu-bar-mode 0))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(assq-delete-all 'continuation fringe-indicator-alist)

(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(windmove-default-keybindings 'meta)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(use-theme dracula)

(use-package faces
  :config
  (set-face-attribute 'default nil
                      :family "JetBrainsMono NF"
                      :weight 'semi-bold
                      :height 140)

  (set-face-attribute 'line-number nil
                      :slant 'normal)

  (set-face-attribute 'line-number-current-line nil
                      :foreground "#BD93F9"))

(provide 'options)
