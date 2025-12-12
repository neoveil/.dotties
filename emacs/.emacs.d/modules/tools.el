;;; tools.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(require 'functions)

(use-feature files
  :bind
  (("C-c o t" . open-temp-buffer)
   ("C-c g h" . go-home)
   ("C-c f p" . find-file-at-point)
   ("C-x C-b" . buffer-menu)
   ("C-c k a" . kill-all-buffers)
   ("C-c k o" . kill-other-buffers)))

(use-feature dired
  :hook
  ((dired-mode  . hl-line-mode)
   (dired-mode  . auto-revert-mode)
   (wdired-mode . disable-hl-line-mode))
  :bind
  (("C-x C-d" . dired)
   (:map dired-mode-map
         (("."   . dired-up-directory)
          ("C-." . dired-omit-mode)
          ("q"   . quit-window-kill-buffer))))
  :config
  (setq-default
   dired-dwim-target t
   dired-listing-switches "-alh --group-directories-first"
   dired-free-space 'separate
   dired-kill-when-opening-new-dired-buffer t)
  (wdired--register-restore-hl-line-mode-on-exit))

(use-feature dired-x
  :config
  (setq-default dired-omit-files (concat dired-omit-files "\\|^\\..+$")))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-subtree-toggle)))

(use-feature which-key
  :diminish
  :bind
  (:map help-map
        ("C-h" . nil))
  :config
  (which-key-mode 1))

(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)
  (add-to-list 'orderless-matching-styles 'orderless-flex)
  (setq-default completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :functions marginalia-mode
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :functions (vertico-mode vertico-mouse-mode vertico-reverse-mode)
  :config
  (setq-default
   vertico-resize t
   vertico-cycle t)
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-reverse-mode 1))

(use-feature vertico-directory
  :after vertico
  :bind
  (:map vertico-map
        (("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package embark
  :bind
  (("C-."   . embark-act)
   ("s-."   . embark-dwim)
   ("C-h b" . embark-bindings)
   ("C-h B" . describe-bindings))
  :config
  (setq-default
   prefix-help-command 'embark-prefix-help-command)
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :bind
  (("C-x b"     . consult-buffer)
   ("C-x 4 b"   . consult-buffer-other-window)
   ("C-x r b"   . consult-bookmark)
   ("M-y"       . consult-yank-pop)
   ("M-g g"     . consult-goto-line)
   ("M-g M-g"   . consult-goto-line)
   ("M-g o"     . consult-outline)
   ("M-g e"     . consult-compile-error)
   ("M-g m"     . consult-mark)
   ("M-g k"     . consult-global-mark)
   ("M-g i"     . consult-imenu)
   ("M-s f"     . consult-fd)
   ("M-s g"     . consult-ripgrep)
   ("M-s l"     . consult-line)
   ("M-s L"     . consult-line-multi)
   ("M-s m"     . consult-man)
   ("M-s i"     . consult-info))
  :config
  (setq-default
   xref-show-xrefs-function 'consult-xref
   xref-show-definitions-function 'consult-xref
   consult-ripgrep-args (concat
                         "rg --hidden --glob !.git/**"
                         (substring consult-ripgrep-args 2))
   consult-fd-args (list
                    (car consult-fd-args)
                    (concat
                     "--hidden "
                     "--exclude .git "
                     (cadr consult-fd-args)))))

(use-package magit
  :bind
  (("C-c m s"   . magit-status)
   ("C-c m l"   . magit-log)
   ("C-c m d"   . magit-diff)
   ("C-c m i"   . magit-init)
   ("C-c m c c" . magit-clone-shallow)
   ("C-c m c f" . magit-clone))
  :config
  (setq-default git-commit-summary-max-length 70))

(use-feature term
  :functions term-handle-exit
  :hook
  (term-mode . disable-display-line-numbers-mode)
  :bind
  (("C-c s c" . shell-command)
   ("C-c s a" . async-shell-command)
   ("C-c t t" . term-zsh)
   ("C-c t o" . term-other-window))
  :config
  (advice-add #'term-handle-exit :around #'term--better-exit-handler-advice))

(use-package anzu
  :diminish
  :functions global-anzu-mode
  :bind
  (([remap query-replace]        . anzu-query-replace)
   ([remap query-replace-regexp] . anzu-query-replace-regexp)
   (:map isearch-mode-map
         (([remap isearch-query-replace]        . anzu-isearch-query-replace)
          ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))))
  :config
  (global-anzu-mode 1))

(use-package ace-link
  :functions ace-link-setup-default
  :config
  (ace-link-setup-default))

(use-package literate-calc-mode
  :mode "\\.calc\\'"
  :bind
  ("C-c o c" . open-literate-calc-temp-buffer))

(provide 'tools)
