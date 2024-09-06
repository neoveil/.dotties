(require 'packages)

(setq-default
 tab-width 2
 indent-tabs-mode nil
 compilation-scroll-output t
 max-lisp-eval-depth 10000)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)

(set-face-attribute 'show-paren-match nil :background "#6272a4")

(defun prog-opts--c-line-comment-style ()
  (interactive)
  (c-toggle-comment-style -1))

(defun prog-opts--word-wrap-on-screen-edge ()
  (interactive)
  (toggle-word-wrap 1))

(use-packages
 yaml-mode
 dockerfile-mode
 json-mode
 systemd
 clojure-mode-extra-font-locking)

(use-package cc-mode
  :hook (c-mode . prog-opts--c-line-comment-style)
  :bind (:map c-mode-map ("C-c C-s" . nil))
  :config (setq-default c-basic-offset 2))
(use-package sh-script
  :config (setq-default sh-basic-offset 2))

(use-package js
  :config
  (setq-default
   tab-width 2
   js-indent-level 2))

(use-package lua-mode
  :ensure t
  :config
  (setq-default
   lua-indent-level 2
   lua-indent-nested-block-content-align nil
   lua-indent-close-paren-align nil))

(use-package python
  :config
  (setq-default
   tab-width 2
   python-indent-offset 2))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . prog-opts--word-wrap-on-screen-edge)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config (setq-default markdown-fontify-code-blocks-natively t))

(use-package grip-mode
  :ensure t
  :config
  (setq-default
   grip-binary-path (concat (getenv "HOME") "/.local/bin/grip")
   grip-update-after-change nil
   grip-sleep-time 0))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . subword-mode))

(use-package cider
  :ensure t
  :hook (cider-mode . eldoc-mode)
  :config
  (setq
   cider-repl-pop-to-buffer-on-connect 1
   cider-show-error-buffer t
   cider-auto-select-error-buffer t
   cider-repl-history-file (concat user-emacs-directory ".cider-history")
   cider-repl-wrap-history t
   cider-prompt-for-symbol nil
   ciderl-repl-use-pretty-printing t
   nrepl-log-messages nil)
  (add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl/cider-middleware"))

(provide 'prog-tools)
