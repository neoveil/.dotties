(require 'packages)

(use-package faces
  :config
  (set-face-attribute 'default nil
                      :family "JetBrainsMono NF"
                      :weight 'semi-bold
                      :height 140)

  (set-face-attribute 'line-number nil
                      :slant 'normal)

  (set-face-attribute 'line-number-current-line nil
                      :foreground "#bd93f9"))


(provide 'fonts)