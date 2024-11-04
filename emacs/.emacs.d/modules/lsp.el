;; -*- lexical-binding: t; -*-

(require 'packages)

(use-feature lsp-booster
  :commands (lsp-booster-setup)
  :after lsp-mode
  :init (lsp-booster-setup))

(provide 'lsp)
