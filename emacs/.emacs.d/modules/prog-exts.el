(require 'packages)

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
  ;; :hook ((clojure-mode . enable-paredit-mode)
  ;;        (emacs-lisp-mode . enable-paredit-mode)
  ;;        (lisp-mode . enable-paredit-mode)
  ;;        (common-lisp-mode . enable-paredit-mode)
  ;;        (scheme-mode . enable-paredit-mode)
  ;;        (cider-repl-mode . enable-paredit-mode))
  :bind (:map paredit-mode-map
              ("M-<down>" . nil)
              ("M-<up>" . nil)
              ("C-<left>" . nil)
              ("C-<right>" . nil)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c e l" . mc/edit-lines)
         ("M-S-<down>" . mc/mark-next-like-this)
         ("M-S-<up>" . mc/mark-previous-like-this)
         ("M-S-<mouse-1>" . mc/add-cursor-on-click)
         ("C-c C->" . mc/mark-all-like-this)
         ("C->" . mc/skip-to-next-like-this)
         ("C-<" . mc/skip-to-previous-like-this)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'prog-exts)
