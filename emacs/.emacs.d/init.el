;; -*- lexical-binding: t; -*-

(defvar modules-directory (file-name-concat user-emacs-directory "modules"))
(defvar local-directory (file-name-concat user-emacs-directory "local"))
(defvar custom-file-path  (file-name-concat modules-directory "custom.el"))

(setq-default custom-file custom-file-path)
(load-file custom-file-path)

(add-to-list 'load-path modules-directory)
(add-to-list 'load-path local-directory)

(require 'packages)
(require 'options)
(require 'ui)
(require 'functions)
(require 'tools)
(require 'prog-tools)
(require 'prog-exts)
(require 'keymaps)
