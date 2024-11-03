;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (file-name-concat user-emacs-directory "modules"))

(require 'packages)
(require 'variables)

(add-to-list 'load-path local-directory)

(require 'options)
(require 'ui)
(require 'functions)
(require 'tools)
(require 'prog-tools)
(require 'prog-exts)
(require 'lsp)
(require 'keymaps)
