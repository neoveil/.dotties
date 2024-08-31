(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(defmacro use-packages (&rest packages)
  `(progn
     ,@(mapcar (lambda (pkg)
                 `(use-package ,pkg :ensure t))
               packages)))

(defmacro use-theme (name)
  `(use-package ,(intern (concat (symbol-name name) "-theme"))
     :ensure t
     :config (load-theme ',name t)))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(provide 'package-init)
