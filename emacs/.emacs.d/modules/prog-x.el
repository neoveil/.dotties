;;; prog-x.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(use-packages
 tldr
 rainbow-mode
 yasnippet-snippets
 string-inflection
 project)

(use-feature eldoc
  :diminish)

(use-package devdocs
  :bind
  (("C-h d"   . nil)
   ("C-h D"   . apropos-documentation)
   ("C-h d l" . devdocs-lookup)
   ("C-h d i" . devdocs-install)
   ("C-h d o" . devdocs-peruse)))

(use-package yasnippet
  :config
  (declare-function yas-global-mode nil)
  (yas-global-mode 1))

(use-package company
  :bind
  (("C-<SPC>"   . company-complete)
   (:map company-active-map
         ("<tab>" . company-complete-selection)))
  :config
  (declare-function global-company-mode nil)
  (setq-default
   company-minimum-prefix-length 2
   company-idle-delay 0
   company-selection-wrap-around t
   company-tooltip-align-annotations t)
  (global-company-mode t))

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
  :after flycheck
  :config
  (declare-function global-flycheck-inline-mode nil)
  (global-flycheck-inline-mode))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (declare-function global-flycheck-eglot-mode nil)
  (global-flycheck-eglot-mode))

(use-package eglot
  :functions eglot--ensure-all
  :init
  (eglot--ensure-all
   '(sh c c++ cmake clojure clojurescript clojurec
     html css json jsonc dockerfile go go-dot-mod
     go-dot-work javascript typescript lua python
     markdown rust vimrc yaml))
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
         ("C-c l t c" . eglot-show-call-hierarchy)
         ("C-c l m"   . imenu)
         ("C-c l h"   . eldoc)))
  :config
  (add-to-list 'eglot-code-action-indications 'mode-line)
  (setq-default
   eglot-autoshutdown t
   eglot-report-progress nil))

(use-package eglot-java
  :after eglot
  :defines eglot-java-mode-map
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
  (setq-default
   eglot-java-workspace-folder (expand-file-name "~/Projects")))

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
