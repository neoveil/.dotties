;;; prog.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(require 'functions)

(setq-default
 tab-width 2
 indent-tabs-mode nil
 max-lisp-eval-depth 10000)

(use-feature isearch
  :bind
  (("C-s"   . isearch-forward-regexp)
   ("C-r"   . isearch-backward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-M-r" . isearch-backward)))

(use-feature replace
  :bind
  (("C-c /" . query-replace-global)
   ("M-%"   . query-replace-regexp)))

(use-feature align
  :bind
  ("M-|" . align-regexp))

(use-feature newcomment
  :bind
  ("C-;" . comment-line))

(use-feature compile
  :bind
  ("C-c c" . compile))

(use-feature elec-pair
  :config
  (electric-pair-mode 1))

(use-feature delsel
  :config
  (delete-selection-mode 1))

(use-feature autorevert
  :diminish auto-revert-mode
  :config
  (global-auto-revert-mode 1))

(use-feature simple
  :hook
  (before-save . delete-trailing-whitespace))

(use-package move-text
  :bind
  (("C-M-<up>"   . move-text-up)
   ("C-M-<down>" . move-text-down)))

(use-package crux
  :bind
  (("C-<return>"   . crux-smart-open-line)
   ("C-S-<return>" . crux-smart-open-line-above)
   ("C-,"          . crux-duplicate-current-line-or-region)
   ("C-c s e"      . crux-sudo-edit)))

(use-feature cc-mode
  :hook
  (c-mode . enable-c-line-comment-style)
  :config
  (setq-default c-basic-offset 2))

(use-feature sh-script
  :config
  (setq-default sh-basic-offset 2))

(use-feature js
  :config
  (setq-default js-indent-level 2))

(use-package lua-mode
  :config
  (setq-default
   lua-indent-level 2
   lua-indent-nested-block-content-align nil
   lua-indent-close-paren-align nil))

(use-feature python
  :config
  (setq-default python-indent-offset 2))

(use-package markdown-mode
  :hook
  (markdown-mode . enable-word-wrap)
  :mode
  (("\\.md\\'"       . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :config
  (setq-default markdown-fontify-code-blocks-natively t))

(use-package clojure-mode
  :hook
  (clojure-mode . subword-mode))

(use-package cider
  :after clojure-mode
  :bind
  ((:map clojure-mode-map
         (("C-c j s" . cider-jack-in)
          ("C-c j c" . cider-connect)))
   (:map cider-mode-map
         ("C-c C-j C-j" . cider-eval-print-last-sexp)))
  :hook
  ((cider-mode . eldoc-mode)
   (cider-repl-mode . subword-mode))
  :config
  (setq-default
   cider-repl-history-file (concat user-emacs-directory ".cider-history")
   cider-repl-wrap-history t
   cider-repl-display-help-banner nil
   cider-allow-jack-in-without-project t
   cider-font-lock-dynamically t
   cider-save-file-on-load t)
  (add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl.middleware/cider-middleware"))

(use-package dockerfile-mode
  :config
  (setq-default dockerfile-indent-offset 2))

(use-feature elisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-j" . eval-print-last-sexp)))

(use-package caddyfile-mode
  :hook
  (caddyfile-mode . set-local-tab-width)
  :mode
  (("Caddyfile\\'"    . caddyfile-mode)
   ("caddy\\.conf\\'" . caddyfile-mode)))

(use-package kotlin-mode
  :config
  (setq-default kotlin-tab-width 2))

(use-package xterm-color
  :config
  (declare-function compilation-filter nil)
  (setq-default
   compilation-environment '("TERM=xterm-256color")
   xterm-color-use-bold-for-bright t
   compilation-scroll-output t)
  (advice-add #'compilation-filter :around #'xterm-color--colorize-compilation-advice))

(use-packages
 git-modes
 yaml-mode
 json-mode
 systemd)

(provide 'prog)
