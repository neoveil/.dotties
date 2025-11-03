;; -*- lexical-binding: t; -*-

(require 'packages)

(use-packages
 yaml-mode
 json-mode
 systemd)

(setq-default
 tab-width 2
 indent-tabs-mode nil
 max-lisp-eval-depth 10000)

(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)
(recentf-mode 1)

(use-feature simple
  :hook
  (before-save . delete-trailing-whitespace))

(use-feature cc-mode
  :hook
  (c-mode . (lambda () (c-toggle-comment-style -1)))
  :config
  (setq-default c-basic-offset 2))

(use-feature sh-script
  :config
  (setq-default sh-basic-offset 2))

(use-feature js
  :config
  (setq-default
   tab-width 2
   js-indent-level 2))

(use-package lua-mode
  :config
  (setq-default
   lua-indent-level 2
   lua-indent-nested-block-content-align nil
   lua-indent-close-paren-align nil))

(use-feature python
  :config
  (setq-default
   tab-width 2
   python-indent-offset 2))

(use-package markdown-mode
  :hook
  (markdown-mode . (lambda () (toggle-word-wrap 1)))
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :config
  (setq-default markdown-fontify-code-blocks-natively t))

(use-package clojure-mode
  :hook
  (clojure-mode . subword-mode))

(use-package cider
  :hook
  (cider-mode . eldoc-mode)
  :config
  (setq-default
   cider-repl-pop-to-buffer-on-connect 1
   cider-show-error-buffer t
   cider-auto-select-error-buffer t
   cider-repl-history-file (concat user-emacs-directory ".cider-history")
   cider-repl-wrap-history t
   cider-prompt-for-symbol nil
   nrepl-log-messages nil)
  (add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl/cider-middleware"))

(use-package dockerfile-mode
  :config
  (setq-default dockerfile-indent-offset 2))

(use-feature elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-j" . eval-print-last-sexp)))

(use-package xterm-color
  :config
  (setq-default
   compilation-environment '("TERM=xterm-256color")
   xterm-color-use-bold-for-bright t
   compilation-scroll-output t)
  (advice-add 'compilation-filter :around (lambda (f p s) (funcall f p (xterm-color-filter s)))))

(provide 'prog-tools)
