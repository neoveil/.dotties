(require 'packages)

(defun term-conf--colorize-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(use-package xterm-color
  :ensure t
  :config
  (setq
   compilation-environment '("TERM=xterm-256color")
   xterm-color-use-bold-for-bright t)
  (advice-add 'compilation-filter :around #'term-conf--colorize-compilation-filter))

(use-package vterm
  :ensure t
  :init (setq-default vterm-always-compile-module t)
  :config (setq-default vterm-max-scrollback 50000))

(use-package vterm-toggle
  :ensure t
  :bind ("C-x C-t" . vterm-toggle)
  :config (setq-default vterm-toggle-hide-method 'reset-window-configration))

(provide 'term-conf)
