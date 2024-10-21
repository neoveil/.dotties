(require 'packages)

(use-packages
 rainbow-mode
 tree-sitter-langs)

(use-package eldoc :diminish)

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package company
  :ensure t
  :config
  (setq-default
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-backends (mapcar
                     (lambda (backend)
                       (if (and (listp backend) (member 'company-yasnippet backend))
                           backend
                         (append (if (consp backend) backend (list backend))
                                 '(:with company-yasnippet))))
                     company-backends))
  (global-company-mode t))

(use-package company-box
  :ensure t
  :diminish
  :hook (company-mode . company-box-mode)
  :config (setq-default company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package paredit
  :ensure t
  :bind
  (:map paredit-mode-map
        ("M-<down>" . nil)
        ("M-<up>" . nil)
        ("C-<left>" . nil)
        ("C-<right>" . nil)))

(use-package multiple-cursors
  :ensure t
  :bind
  ("C-c e l" . mc/edit-lines)
  ("C-d" . mc/mark-next-like-this)
  ("M-S-<mouse-1>" . mc/add-cursor-on-click)
  ("C-c C->" . mc/mark-all-like-this)
  ("C->" . mc/skip-to-next-like-this)
  ("C-<" . mc/unmark-next-like-this))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :ensure t
  :hook
  (after-init . global-tree-sitter-mode)
  (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (set-face-foreground 'tree-sitter-hl-face:function.macro "#ff79c6")
  (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq-default
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(provide 'prog-exts)
