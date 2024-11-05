;; -*- lexical-binding: t; -*-

(require 'packages)

(use-packages lsp-ui)

(use-package lsp-mode
  :commands
  (lsp lsp-deferred lsp-ensure-server dap-auto-configure-mode)
  :defer t
  :hook
  (prog-mode . lsp-deferred)
  (nxml-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq-default
   lsp-keymap-prefix "C-c l"
   lsp-use-plists t
   lsp-warn-no-matched-clients nil
   lsp-completion-provider :none
   lsp-headerline-breadcrumb-enable nil
   lsp-restart 'ignore
   lsp-idle-delay 0.1
   lsp-enable-on-type-formatting nil
   lsp-xml-bin-file "/usr/bin/lemminx"
   lsp-xml-jar-file "/usr/share/java/lemminx/lemminx-0.28.0.jar"
   lsp-sql-server-path "/usr/bin/sql-language-server"
   lsp-clients-lua-language-server-bin "/usr/lib/lua-language-server/bin/lua-language-server"
   lsp-clients-lua-language-server-main-location "/usr/lib/lua-language-server/main.lua"))

(use-feature lsp-lens
  :after lsp-mode
  :hook (lsp-mode . lsp-lens-mode)
  :diminish)

(use-package lsp-haskell
  :after lsp-mode)

(use-package lsp-pyright
  :after lsp-mode)

(use-package lsp-jedi
  :after lsp-mode)

(use-package lsp-java
  :after lsp-mode
  :hook (java-mode . lsp-java-lens-mode)
  :bind
  (:map java-mode-map
        ("C-c l j i o" . lsp-java-organize-imports)
        ("C-c l j i a" . lsp-java-add-import)
        ("C-c l j b" . lsp-java-build-project)
        ("C-c l j g u" . lsp-java-add-unimplemented-methods)
        ("C-c l j g s" . lsp-java-generate-to-string)
        ("C-c l j g e" . lsp-java-generate-equals-and-hash-code)
        ("C-c l j g o" . lsp-java-generate-overrides)
        ("C-c l j g a" . lsp-java-generate-getters-and-setters)
        ("C-c l j t b" . lsp-jt-browser)
        ("C-c l j t r" . lsp-jt-report-open)
        ("C-c l j d" . lsp-treemacs-java-deps-list))
  :config
  (setq-default
   lsp-java-maven-download-sources t
   lsp-java-references-code-lens-enabled t
   lsp-java-implementations-code-lens-enabled t
   lsp-java-content-provider-preferred "fernflower"
   lsp-java-max-concurrent-builds 4
   lsp-java-compile-null-analysis-mode "automatic")
  (lsp-ensure-server 'jdtls))

(use-feature lsp-java-boot
  :diminish lsp-java-boot-lens-mode
  :after lsp-java
  :hook (java-mode . lsp-java-boot-lens-mode))

(use-feature lsp-java-x
  :commands (lsp-java-lombok-setup lsp-java-replace-vmargs)
  :after lsp-java
  :config
  (lsp-java-lombok-setup)
  (lsp-java-replace-vmargs '(("-Xmx" . "4G"))))

(use-feature lsp-booster
  :commands (lsp-booster-setup)
  :after lsp-mode
  :config (lsp-booster-setup))

(provide 'lsp)
