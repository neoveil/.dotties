(require 'packages)

(use-packages
 yaml-mode
 json-mode
 systemd)

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
(recentf-mode 1)
(crux-reopen-as-root-mode 1)

(use-feature cc-mode
  :commands (c-toggle-comment-style)
  :hook (c-mode . (lambda () (c-toggle-comment-style -1)))
  :config (setq-default c-basic-offset 2))

(use-feature sh-script
  :config (setq-default sh-basic-offset 2))

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
  :config
  (setq-default
   grip-update-after-change nil
   grip-sleep-time 0
   grip-theme 'dark))

(use-package clojure-mode
  :hook (clojure-mode . subword-mode))

(use-package cider
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

(use-package dockerfile-mode
  :config (setq-default dockerfile-indent-offset 2))

(use-package xterm-color
  :commands (xterm-color-filter)
  :config
  (setq-default
   compilation-environment '("TERM=xterm-256color")
   xterm-color-use-bold-for-bright t
   compilation-scroll-output t)
  (advice-add 'compilation-filter :around (lambda (f p s) (funcall f p (xterm-color-filter s)))))

(use-package quickrun
  :bind ("C-c r r" . quickrun)
  :config
  (setq-default
   quickrun-focus-p nil
   quickrun-timeout-seconds nil))

(provide 'prog-tools)
