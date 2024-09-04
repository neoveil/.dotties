(require 'package-init)

(use-packages
 dash
 vterm
 clojure-mode-extra-font-locking
 yaml-mode
 dockerfile-mode
 json-mode
 systemd)

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package pinentry
  :ensure t
  :config (pinentry-start))

(use-package move-text
  :ensure t
  :bind (("C-M-<up>" . move-text-up)
         ("C-M-<down>" . move-text-down)))

(use-package dired
  :bind (:map dired-mode-map
              ("C-." . dired-up-directory)
              ("C-x C-." . dired-up-directory))
  :config (setq-default
           dired-dwim-target t
           dired-listing-switches "-alh --group-directories-first"
           dired-free-space 'separate))

(use-package magit
  :ensure t
  :bind (("C-c m s" . magit-status)
         ("C-c m l" . magit-log))
  :config (setq-default git-commit-summary-max-length 70))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-c C-c M-x" . execute-extended-command))
  :config (setq-default smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (setq-default yas/triggers-in-field nil)
  (yas-global-mode 1))

(use-package company
  :ensure t
  :config (global-company-mode t))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (common-lisp-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("M-<down>" . nil)
              ("M-<up>" . nil)
              ("C-<left>" . nil)
              ("C-<right>" . nil))
  :config (show-paren-mode t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c e l" . mc/edit-lines)
         ("M-S-<down>" . mc/mark-next-like-this)
         ("M-S-<up>" . mc/mark-previous-like-this)
         ("M-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C->" . mc/skip-to-next-like-this)
         ("C-<" . mc/skip-to-previous-like-this)))

(use-package sudo-edit
  :ensure t
  :defer t
  :bind ("C-c f s" . sudo-edit))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode . rainbow-mode))

(use-package xterm-color
  :ensure t
  :config
  (setq
   compilation-environment '("TERM=xterm-256color")
   xterm-color-use-bold-for-bright t)

  (defun colorize-advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))

  (advice-add 'compilation-filter :around #'colorize-advice-compilation-filter))

(use-package vterm-toggle
  :ensure t
  :bind ("C-x C-t" . vterm-toggle)
  :config
  (setq-default vterm-toggle-hide-method 'reset-window-configration)

  ;; make the buffer fullscreen
  ;; (setq-default vterm-toggle-fullscreen-p nil)

  ;; show buffer in current window
  ;; (add-to-list 'display-buffer-alist
  ;;              '((lambda (buffer-or-name _)
  ;;                  (let ((buffer (get-buffer buffer-or-name)))
  ;;                    (with-current-buffer buffer
  ;;                      (or (equal major-mode 'vterm-mode)
  ;;                          (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
  ;;                (display-buffer-reuse-window display-buffer-same-window))))

  ;; show buffer in bottom side
  ;; (add-to-list 'display-buffer-alist
  ;;              '((lambda (buffer-or-name _)
  ;;                  (let ((buffer (get-buffer buffer-or-name)))
  ;;                    (with-current-buffer buffer
  ;;                      (or (equal major-mode 'vterm-mode)
  ;;                          (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
  ;;                (display-buffer-reuse-window display-buffer-at-bottom)
  ;;                (reusable-frames . visible)
  ;;                (window-height . 0.3)))
  )

(use-package cc-mode
  :hook (c-mode . (lambda ()
                    (interactive)
                    (c-toggle-comment-style -1)))
  :config (setq-default
           c-basic-offset 2
           c-default-style '((java-mode . "java")
                             (awk-mode . "awk")
                             (other . "bsd"))))

(use-package sh-script
  :config (setq-default sh-basic-offset 2))

(use-package js
  :config (setq-default
           tab-width 2
           js-indent-level 2))

(use-package lua-mode
  :config (setq-default
           lua-indent-level 2
           lua-indent-nested-block-content-align nil
           lua-indent-close-paren-align nil))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . (lambda ()
                           (interactive)
                           (toggle-word-wrap 1)))
  :config (setq-default
           markdown-fontify-code-blocks-natively t
           markdown-command "cmark-gfm"
           markdown-css-paths '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.6.1/github-markdown-dark.css"
                                "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.10.0/styles/github-dark.min.css")
           markdown-xhtml-header-content "<style>.markdown-body{box-sizing:border-box;min-width:200px;max-width:980px;margin:0 auto;padding:45px}@media (max-width:767px){.markdown-body{padding:15px}}</style><script src=https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.10.0/highlight.min.js></script><script>document.addEventListener(\"DOMContentLoaded\",(()=>document.body.classList.add(\"markdown-body\")));</script><script>hljs.highlightAll()</script>"))

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

(provide 'packages)
