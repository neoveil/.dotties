;;; misc.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'packages))

(use-package google-translate
  :bind
  ("C-c g t" . google-translate-smooth-translate)
  :config
  (setq-default
   google-translate-translation-directions-alist '(("en" . "pt") ("pt" . "en") ("en" . "en"))
   google-translate-pop-up-buffer-set-focus t))

(provide 'misc)
