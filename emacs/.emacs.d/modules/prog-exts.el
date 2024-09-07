(require 'packages)

(use-packages
 rainbow-mode
 tree-sitter-langs)

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
  :bind
  (:map paredit-mode-map
        ("M-<down>" . nil)
        ("M-<up>" . nil)
        ("C-<left>" . nil)
        ("C-<right>" . nil)))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c e l" . mc/edit-lines)
   ("M-S-<down>" . mc/mark-next-like-this)
   ("M-S-<up>" . mc/mark-previous-like-this)
   ("M-S-<mouse-1>" . mc/add-cursor-on-click)
   ("C-c C->" . mc/mark-all-like-this)
   ("C->" . mc/skip-to-next-like-this)
   ("C-<" . mc/unmark-next-like-this)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tree-sitter
  :ensure t
  :hook
  ((after-init . global-tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode))
  :config
  (set-face-foreground 'tree-sitter-hl-face:function.macro "#ff79c6")
  (set-face-attribute 'tree-sitter-hl-face:variable.parameter nil
                      :weight 'semi-bold))

(provide 'prog-exts)
