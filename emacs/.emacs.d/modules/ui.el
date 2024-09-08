(require 'packages)

(defun set-font (frame)
  (with-selected-frame frame
    (set-face-attribute 'default frame
                        :family "JetBrainsMono NF"
                        :weight (if (display-graphic-p frame) 'semi-bold 'normal)
                        :height 140)))

(use-package faces
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions 'set-font)
    (set-font (selected-frame)))

  (set-face-attribute 'line-number nil
                      :slant 'normal)

  (set-face-attribute 'line-number-current-line nil
                      :foreground "#bd93f9"))

(provide 'ui)
