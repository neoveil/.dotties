(require 'packages)

(use-packages
 yaml-mode
 dockerfile-mode
 json-mode
 systemd
 clojure-mode-extra-font-locking)

(setq-default
 tab-width 2
 indent-tabs-mode nil
 max-lisp-eval-depth 10000)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(save-place-mode 1)

(use-package cc-mode
  :hook
  (c-mode . (lambda ()
              (declare-function c-toggle-comment-style "cc-cmds")
              (c-toggle-comment-style -1)))
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
  :hook (markdown-mode . (lambda () (toggle-word-wrap 1)))
  :bind
  (:map markdown-mode-map
        ("C-c g s" . grip-start-preview)
        ("C-c g d" . grip-stop-preview)
        ("C-c g r" . grip-restart-preview)
        ("C-c g g" . grip-browse-preview))
  :mode
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
  ("README\\.md\\'" . gfm-mode)
  :config
  (setq-default markdown-fontify-code-blocks-natively t)
  (dolist (face '(markdown-header-face-1
                  markdown-header-face-2
                  markdown-header-face-3))
    (set-face-attribute face nil :height 'unspecified)))

(use-package grip-mode
  :ensure t
  :config
  (setq-default
   grip-update-after-change nil
   grip-sleep-time 0))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . subword-mode))

(use-package cider
  :ensure t
  :hook (cider-mode . eldoc-mode)
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

(use-package xterm-color
  :ensure t
  :config
  (setq-default
   compilation-environment '("TERM=xterm-256color")
   xterm-color-use-bold-for-bright t
   compilation-scroll-output t)
  (advice-add 'compilation-filter :around (lambda (f p s) (funcall f p (xterm-color-filter s)))))

(provide 'prog-tools)
