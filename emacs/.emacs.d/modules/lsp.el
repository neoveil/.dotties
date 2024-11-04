;; -*- lexical-binding: t; -*-

(require 'packages)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :defer t
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq-default
   lsp-keymap-prefix "C-c l"
   lsp-use-plists t
   lsp-warn-no-matched-clients nil
   lsp-completion-provider :none
   lsp-headerline-breadcrumb-enable nil))

(use-package lsp-haskell
  :after lsp-mode)

(use-package lsp-pyright
  :after lsp-mode)

(use-package lsp-jedi
  :after lsp-mode)

(defun lsp-java--replace-vmargs (pairs)
  (defvar lsp-java-vmargs)
  (dolist (pair pairs)
    (let* ((prefix (car pair))
           (new-value (cdr pair))
           (full-arg (concat prefix new-value)))
      (setq-default lsp-java-vmargs
                    (mapcar (lambda (arg)
                              (if (string-prefix-p prefix arg)
                                  full-arg
                                arg))
                            lsp-java-vmargs)))))

(use-package lsp-java
  :after lsp-mode
  :config
  (setq-default
   lsp-java-maven-download-sources t)
  (lsp-java--replace-vmargs '(("-Xmx" . "4G"))))

(use-feature lsp-java-boot
  :after lsp-java
  :hook (java-mode . lsp-java-boot-lens-mode))

(use-feature lsp-java-lombok
  :commands (lsp-java-lombok-setup)
  :after lsp-java
  :config (lsp-java-lombok-setup))

(use-feature lsp-booster
  :commands (lsp-booster-setup)
  :after lsp-mode
  :config (lsp-booster-setup))

(provide 'lsp)
