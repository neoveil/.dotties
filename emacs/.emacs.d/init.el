;;; init.el -*- lexical-binding: t; -*-

;; Author: Lucas A. F. Vitor <luca.oetdbem@gmail.com>
;; Created: 2025-11-21

;; actually it was created long before, maybe 1-2 years before
;; but i decided to include these now so i can reference it
;; in the future and be somewhat nostalgic (:

(defvar modules-directory
  (file-name-concat user-emacs-directory "modules")
  "Directory containing modular Emacs configuration files")

(defvar local-directory
  (file-name-concat user-emacs-directory "local")
  "Directory for local modules/configuration files")

(defvar custom-file-path
  (file-name-concat modules-directory "custom.el")
  "Path to the file used to store customized variables")

(setq-default custom-file custom-file-path)
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path modules-directory)
(add-to-list 'load-path local-directory)

;; TODO:
;;   setup lsp?
;;   setup org-mode
;;   drop ido and company - replace for ivy, vertico, orderless, consult, marginalia, helm, corfu, cape

(require 'packages)
(require 'options)
(require 'ui)
(require 'functions)
(require 'tools)
(require 'prog)
(require 'prog-x)
(require 'misc)
