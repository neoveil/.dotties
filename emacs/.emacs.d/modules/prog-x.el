;;; prog-x.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(require 'functions)

(use-packages
 tldr
 rainbow-mode
 yasnippet-snippets
 yasnippet-capf
 string-inflection
 project)

(use-feature eldoc
  :diminish
  :hook
  (special-mode . eldoc--disable-line-numbers)
  :bind
  (("C-c h l" . eldoc)
   ("C-c h h" . eldoc--snapshot)))

(use-package devdocs
  :bind
  (("C-h d"   . nil)
   ("C-h D"   . apropos-documentation)
   ("C-h d l" . devdocs-lookup)
   ("C-h d i" . devdocs-install)
   ("C-h d o" . devdocs-peruse)))

(use-package yasnippet
  :functions yas-global-mode
  :config
  (yas-global-mode 1))

(use-package corfu
  :functions global-corfu-mode
  :init
  (global-corfu-mode 1)
  :bind
  (("C-<SPC>"   . completion-at-point)
   ("C-S-<SPC>" . set-mark-command)
   (:map corfu-map
         ("RET" . nil)))
  :config
  (setq-default
   corfu-count 15
   corfu-cycle t
   corfu-preselect 'first
   corfu-preview-current nil
   corfu-auto t
   corfu-auto-delay 0
   corfu-auto-prefix 2
   corfu-auto-trigger "."))

(use-feature corfu-echo
  :after corfu
  :functions corfu-echo-mode
  :init
  (corfu-echo-mode 1)
  :config
  (setq-default corfu-echo-delay 0))

(use-feature corfu-history
  :after corfu
  :functions corfu-history-mode
  :init
  (corfu-history-mode 1))

(use-feature corfu-popupinfo
  :after corfu
  :functions corfu-popupinfo-mode
  :init
  (corfu-popupinfo-mode 1)
  :config
  (setq-default
   corfu-popupinfo-delay '(nil . 0)
   corfu-popupinfo-max-height 30
   corfu-popupinfo-max-width 150))

(use-package nerd-icons-corfu
  :after corfu
  :functions nerd-icons-corfu-formatter
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :after (corfu eglot)
  :bind
  ("C-c p" . cape-prefix-map)
  :hook
  ((prog-mode text-mode conf-mode temp-mode literal-calc-mode eglot-managed-mode) . cape--setup-capf)
  :init
  (advice-add 'eglot-completion-at-point :around 'cape-wrap-buster))

(use-package paredit
  :bind
  (:map paredit-mode-map
        (("M-<down>"  . nil)
         ("M-<up>"    . nil)
         ("C-<left>"  . nil)
         ("C-<right>" . nil))))

(use-package multiple-cursors
  :bind
  (("C-c e l"          . mc/edit-lines)
   ("C->"              . mc/mark-next-like-this)
   ("C-<"              . mc/unmark-next-like-this)
   ("M-<down-mouse-1>" . nil)
   ("M-<mouse-1>"      . mc/add-cursor-on-click)
   ("C-c C->"          . mc/mark-all-like-this)
   ("C-M->"            . mc/skip-to-next-like-this)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package flycheck
  :hook
  (after-init . global-flycheck-mode)
  :config
  (setq-default
   flycheck-disabled-checkers '(emacs-lisp-checkdoc)
   flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-inline
  :functions global-flycheck-inline-mode
  :after flycheck
  :config
  (global-flycheck-inline-mode))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :functions global-flycheck-eglot-mode
  :config
  (global-flycheck-eglot-mode))

(use-package eglot
  :init
  (eglot--ensure-all
   '(sh c c++ cmake clojure clojurescript clojurec
     html css json jsonc dockerfile go go-dot-mod
     go-dot-work js typescript lua python rust
     markdown vimrc yaml))
  :bind
  (:map eglot-mode-map
        (("C-c l s s" . eglot-shutdown)
         ("C-c l s a" . eglot-shutdown-all)
         ("C-c l s r" . eglot-reconnect)
         ("C-c l r"   . eglot-rename)
         ("C-c l f"   . eglot-format)
         ("C-c l a a" . eglot-code-actions)
         ("C-c l a o" . eglot-code-action-organize-imports)
         ("C-c l a q" . eglot-code-action-quickfix)
         ("C-c l a e" . eglot-code-action-extract)
         ("C-c l a i" . eglot-code-action-inline)
         ("C-c l a r" . eglot-code-action-rewrite)
         ("C-c l t s" . eglot-show-type-hierarchy)
         ("C-c l t c" . eglot-show-call-hierarchy)))
  :config
  (add-to-list 'eglot-code-action-indications 'mode-line)
  (setq-default
   eglot-autoshutdown t
   eglot-report-progress nil))

(use-package eglot-java
  :after eglot
  :hook
  (java-mode . eglot-java-mode)
  :bind
  (:map eglot-java-mode-map
        (("C-c l j n" . eglot-java-file-new)
         ("C-c l j x" . eglot-java-run-main)
         ("C-c l j t" . eglot-java-run-test)
         ("C-c l j r" . eglot-java-project-build-refresh)
         ("C-c l j b" . eglot-java-project-build-task)))
  :config
  (eglot-java--set-jdtls-xmx "4G"))

(use-feature eglot-java-lombok
  :after eglot-java
  :functions eglot-java-lombok-ensure
  :config
  (eglot-java-lombok-ensure))

;; (use-package copilot
;;   :diminish
;;   :hook
;;   ((text-mode . copilot-mode)
;;    (prog-mode . copilot-mode))
;;   :bind
;;   (:map copilot-completion-map
;;         ("<tab>" . copilot-accept-completion))
;;   :config
;;   (setq-default
;;    copilot-indent-offset-warning-disable t
;;    copilot-max-char -1))

(provide 'prog-x)
