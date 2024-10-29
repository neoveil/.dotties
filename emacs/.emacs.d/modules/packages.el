(setq-default
 straight-repository-branch "develop"
 straight-process-buffer " *straight-process*"
 straight-check-for-modifications (if (and (executable-find "watchexec")
                                           (executable-find "python3"))
                                      '(watch-files find-when-checking)
                                    '(find-at-startup find-when-checking)))

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
   straight-host-usernames '((github . "lucasarthur"))
   straight-vc-git-default-clone-depth 1))

(defmacro use-feature (name &rest args)
  (declare (indent defun))
  `(use-package ,name
     :straight (:type built-in)
     ,@args))

(defmacro use-packages (&rest packages)
  `(progn ,@(mapcar (lambda (pkg) `(use-package ,pkg)) packages)))

(defmacro use-theme (name &rest settings)
  `(use-package ,(intern (concat (symbol-name name) "-theme"))
     :config
     (progn
       (load-theme ',name t)
       ,@(when settings
           (mapcar (lambda (setting)
                     `(setq-default ,(car setting) ,(cadr setting)))
                   (seq-partition settings 2))))))

(provide 'packages)
