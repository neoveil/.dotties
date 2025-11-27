;;; packages.el -*- lexical-binding: t; -*-

(setq-default
 straight-repository-branch "develop"
 straight-process-buffer " *straight-process*"
 straight-check-for-modifications '(watch-files find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(declare-function straight-use-package nil)
(straight-use-package 'use-package)

(use-package straight
  :config
  (setq-default
   use-package-always-demand t
   straight-use-package-by-default t
   straight-host-usernames '((github . "neoveil"))
   straight-vc-git-default-clone-depth 1))

(defmacro use-feature (name &rest args)
  "Declare configuration for a built-in feature NAME via `use-package'.

This macro is a convenience wrapper around `use-package' for
packages that ship with Emacs.  It expands into a `use-package'
form with `:straight (:type built-in)' already inserted, allowing
the remaining ARGS to specify additional keywords such as :bind,
:config, :hook, etc.

Example:
  (use-feature simple
    :bind (\"C-x C-b\" . buffer-menu))

expands into:
  (use-package simple
    :straight (:type built-in)
    :bind (\"C-x C-b\" . buffer-menu))"
  (declare (indent defun))
  `(use-package ,name
     :straight (:type built-in)
     ,@args))

(defmacro use-packages (&rest packages)
  "Load multiple PACKAGES using `use-package' in a compact form.

Each element of PACKAGES should be a symbol naming a package.
This macro expands into a PROGN form containing one `use-package'
call per package, with no additional keywords.

Example:
  (use-packages foo bar baz)

expands into:
  (progn
    (use-package foo)
    (use-package bar)
    (use-package baz))"
  `(progn ,@(mapcar (lambda (pkg) `(use-package ,pkg)) packages)))

(use-packages
 dash
 diminish)

(provide 'packages)
