;; -*- lexical-binding: t; -*-

(require 'packages)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq-default
   lsp-keymap-prefix "C-c l"
   lsp-use-plists t
   lsp-log-io nil
   lsp-warn-no-matched-clients nil))

(use-feature lsp-booster
  :commands (lsp-booster-setup)
  :after lsp-mode
  :config (lsp-booster-setup))

(provide 'lsp)
