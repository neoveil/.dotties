(require 'variables)
(require 'packages)

(load-file custom-file-path)

;; basic settings
(setq-default
 inhibit-startup-screen t
 custom-file custom-file-path
 blink-cursor-blinks 0
 isearch-wrap-pause 'no-ding
 enable-recursive-minibuffers t
 ring-bell-function 'ignore
 confirm-kill-emacs 'y-or-n-p
 native-comp-async-report-warnings-errors 'silent)

;; files settings
(setq-default
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 require-final-newline t
 fill-column 120
 vc-follow-symlinks t
 confirm-nonexistent-file-or-buffer nil)

;; clipboard settings
(setq-default
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t)

;; ui
(unless (eq system-type 'darwin) (menu-bar-mode 0))
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(assq-delete-all 'continuation fringe-indicator-alist)

;; misc
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(alpha-background . 90))
(windmove-default-keybindings 'meta)

(use-theme
 dracula
 dracula-use-24-bit-colors-on-256-colors-terms t)

(provide 'options)
