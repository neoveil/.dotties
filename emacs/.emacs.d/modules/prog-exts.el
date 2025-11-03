;; -*- lexical-binding: t; -*-

(require 'packages)

(use-packages
 tldr
 rainbow-mode
 yasnippet-snippets
 string-inflection)

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

(provide 'prog-exts)
