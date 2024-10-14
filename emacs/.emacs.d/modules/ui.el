(require 'packages)

(use-package faces
  :config
  (set-face-attribute 'default nil :family "JetBrainsMono NF" :height 150)
  (set-face-attribute 'line-number nil :slant 'normal)
  (set-face-attribute 'line-number-current-line nil :foreground "#bd93f9"))

(provide 'ui)
