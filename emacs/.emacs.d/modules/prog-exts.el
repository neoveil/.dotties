;; -*- lexical-binding: t; -*-

(require 'packages)

(use-packages
 tldr
 rainbow-mode
 yasnippet-snippets
 string-inflection
 tree-sitter-langs
 flycheck-projectile
 flycheck-pkg-config
 flycheck-pyflakes
 flycheck-clj-kondo)

(use-feature eldoc
  :diminish)

(use-package devdocs
  :bind
  (("C-h d" . nil)
   ("C-h D" . apropos-documentation)
   ("C-h d l" . devdocs-lookup)
   ("C-h d i" . devdocs-install)
   ("C-h d o" . devdocs-peruse)))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(defun company--get-backends-with-yasnippet-appended ()
  (mapcar
   (lambda (backend)
     (if (and (listp backend) (member 'company-yasnippet backend))
         backend
       (append (if (consp backend) backend (list backend))
               '(:with company-yasnippet))))
   company-backends))

(use-package company
  :bind
  ((:map company-active-map
         ("<tab>" . company-complete-selection))
   ("M-/" . 'company-complete))
  :config
  (setq-default
   company-minimum-prefix-length 1
   company-idle-delay 0.0
   company-selection-wrap-around t
   company-tooltip-align-annotations t
   company-backends (company--get-backends-with-yasnippet-appended))
  (global-company-mode t))

(use-package company-box
  :diminish
  :hook
  (company-mode . company-box-mode)
  :config
  (setq-default
   company-box-icons-alist 'company-box-icons-all-the-icons
   company-box-scrollbar nil))

(use-package paredit
  :bind
  (:map paredit-mode-map
        (("M-<down>" . nil)
         ("M-<up>" . nil)
         ("C-<left>" . nil)
         ("C-<right>" . nil))))

(use-package multiple-cursors
  :bind
  (("C-c e l" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("M-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-S->" . mc/skip-to-next-like-this)
   ("C-<" . mc/unmark-next-like-this)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(defun tree-sitter--ignore-langs (&rest langs)
  (setq-default
   tree-sitter-major-mode-language-alist
   (cl-remove-if
    (lambda (pair) (member (cdr pair) langs))
    tree-sitter-major-mode-language-alist)))

(use-package tree-sitter
  :hook
  ((after-init . global-tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (tree-sitter--ignore-langs 'dockerfile))

(use-package flycheck
  :config
  (setq-default
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (global-flycheck-mode))

(use-package flycheck-inline
  :after flycheck
  :hook
  (flycheck-mode . flycheck-inline-mode))

(use-package flycheck-pycheckers
  :hook
  (flycheck-mode . flycheck-pycheckers-setup))

(use-package flycheck-checkbashisms
  :hook
  (flycheck-mode . flycheck-checkbashisms-setup))

(use-package flycheck-rust
  :hook
  (flycheck-mode . flycheck-rust-setup))

(use-package flycheck-kotlin
  :hook
  (flycheck-mode . flycheck-kotlin-setup))

(provide 'prog-exts)
