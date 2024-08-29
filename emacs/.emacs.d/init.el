(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(defvar custom-file-path "~/.emacs.d/custom.el")
(defvar local-directory "~/.emacs.d/local/")

(add-to-list 'load-path local-directory)
(load-file custom-file-path)

(setq-default
 inhibit-splah-screen t
 create-lockfiles nil
 make-backup-files nil
 custom-file custom-file-path
 auto-save-default nil
 fill-column 120
 blink-cursor-blinks 0
 tab-width 2
 indent-tabs-mode nil
 enable-recursive-minibuffers t
 select-enable-primary t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point t
 apropos-do-all t
 uniquify-buffer-name-style 'forward
 ring-bell-function 'ignore
 tramp-auto-save-directory "/tmp")

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'default-frame-alist '(alpha-background . 90))

(windmove-default-keybindings 'meta)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)

(defun put-buffer-name-on-clipboard ()
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))

(defun put-file-name-on-clipboard ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))

(defun load-path-here ()
  (interactive)
  (add-to-list 'load-path default-directory))

(defun duplicate-line-down ()
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'duplicate-line-down)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-M-=") 'global-text-scale-adjust)

(require 'use-package)

(defmacro use-packages (&rest packages)
  `(progn
     ,@(mapcar (lambda (pkg)
                 `(use-package ,pkg :ensure t))
               packages)))

(defmacro use-theme (name)
  `(use-package ,(intern (concat (symbol-name name) "-theme"))
     :ensure t
     :config (load-theme ',name t)))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-theme dracula)

(use-package xterm-color
  :ensure t
  :config
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun colorize-advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'colorize-advice-compilation-filter))

(use-package faces
  :config (set-face-attribute 'default nil
                              :family "JetBrains Mono NF"
                              :weight 'semi-bold
                              :height 130))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures
   'prog-mode
   '("--" "---" "==" "===" "!=" "!==" "=!="
     "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
     "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
     "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
     "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
     "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
     "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
     "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
     "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
     "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
     "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
     ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
     "<:<" ";;;"))
  (global-ligature-mode t))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :ensure t
  :config (when (memq window-system '(mac ns)) (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

(use-package magit
  :ensure t
  :bind (("C-c m s" . magit-status)
         ("C-c m l" . magit-log)))

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

(use-package paredit
  :ensure t
  :hook ((clojure-mode . enable-paredit-mode)
         (cider-repl-mode . enable-paredit-mode)
         (emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (ielm-mode . enable-paredit-mode)
         (common-lisp-mode . enable-paredit-mode)
         (scheme-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup . enable-paredit-mode)
         (haskell-mode . enable-paredit-mode)
         (haskell-interactive-mode . enable-paredit-mode))
   :bind (("M-[" . paredit-wrap-square)
          ("M-{" . paredit-wrap-curly))
   :config (show-paren-mode t))

(use-package multiple-cursors
  :ensure t
  :bind (("C-c l" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-\"" . mc/skip-to-next-like-this)
         ("C-:" . mc/skip-to-previous-like-this)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . (lambda ()
                           (interactive)
                           (toggle-word-wrap 1))))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-triggers-in-field nil)
  (yas-global-mode 1))

(use-package company
  :ensure t
  :bind (:map
         global-map
         ("M-/" . company-complete-common-or-cycle)

         :map
         company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq
   company-frontends '(company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend)
   company-idle-delay 0.1)
  (global-company-mode t))

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'subword-mode)

  (defun prettify-fns ()
    (font-lock-add-keywords
     nil `(("(\\(fn\\)[[:space:]]"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      "ƒ")
                      nil))))))

  (defun prettify-anonymous-fns ()
    (font-lock-add-keywords
     nil `(("\\(#\\)("
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

  (defun prettify-sets ()
    (font-lock-add-keywords
     nil `(("\\(#\\){"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      "∈")
                      nil))))))

  (add-hook 'clojure-mode-hook 'prettify-fns)
  (add-hook 'clojure-mode-hook 'prettify-anonymous-fns)
  (add-hook 'clojure-mode-hook 'prettify-sets)
  (add-hook 'cider-repl-mode-hook 'prettify-fns)
  (add-hook 'cider-repl-mode-hook 'prettify-anonymous-fns)
  (add-hook 'cider-repl-mode-hook 'prettify-sets))

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

(use-package tree-sitter
  :ensure t
  :config (global-tree-sitter-mode 1))

(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode))

(use-packages
 dash
 clojure-mode-extra-font-locking
 tree-sitter-langs
 scala-mode
 yaml-mode
 glsl-mode
 lua-mode
 cmake-mode
 rust-mode
 csharp-mode
 dockerfile-mode
 toml-mode
 nginx-mode
 kotlin-mode
 go-mode
 haskell-mode
 hindent
 typescript-mode
 json-mode)

;; misc
(setq-default
 c-basic-offset 2
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (other . "bsd")))

(add-hook 'c-mode-hook (lambda ()
                         (interactive)
                         (c-toggle-comment-style -1)))
