(require 'package-init)

(use-packages
 dash
 vterm
 clojure-mode-extra-font-locking
 yaml-mode
 lua-mode
 dockerfile-mode
 json-mode)

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config (when (memq window-system '(mac ns)) (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind (("C-M-<up>" . move-text-up)
         ("C-M-<down>" . move-text-down)))

(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status)
  :config (setq git-commit-summary-max-length 70))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-c C-c M-x" . execute-extended-command))
  :config (setq smex-save-file (concat user-emacs-directory ".smex-items")))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-mode 1)
  (ido-everywhere 1)
  (ido-ubiquitous-mode 1))

(use-package yasnippet
  :ensure t
  :config
  (setq yas/triggers-in-field nil)
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
              ("M-<up>" . nil))
  :config (show-paren-mode t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c l" . mc/edit-lines)
         ("C-S-d" . mc/mark-next-like-this)
         ("M-S-<down>" . mc/mark-next-like-this)
         ("M-S-<up>" . mc/mark-previous-like-this)
         ("M-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C->" . mc/skip-to-next-like-this)
         ("C-<" . mc/skip-to-previous-like-this)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  (setq vterm-toggle-hide-method 'reset-window-configration)

  ;; make the buffer fullscreen
  ;; (setq vterm-toggle-fullscreen-p nil)

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


(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . (lambda ()
                           (interactive)
                           (toggle-word-wrap 1))))

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
