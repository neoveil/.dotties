(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(require 'use-package)

(defmacro use-packages (&rest packages)
  `(progn ,@(mapcar (lambda (pkg) `(use-package ,pkg :ensure t)) packages)))

(defmacro use-theme (name &rest settings)
  `(use-package ,(intern (concat (symbol-name name) "-theme"))
     :ensure t
     :config
     (progn
       (load-theme ',name t)
       ,@(when settings
           (mapcar (lambda (setting)
                     `(setq-default ,(car setting) ,(cadr setting)))
                   (seq-partition settings 2))))))

(use-package auto-package-update
  :ensure t
  :config
  (setq-default
   auto-package-update-interval 3
   auto-package-update-delete-old-versions t
   auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide 'packages)
