;; idea and chunks of code stolen from treemacs eldoc display

(defcustom neotree-display-file-info t
  "*If non-nil, display metadata of the file path at point on minibuffer."
  :type 'boolean
  :group 'neotree)

(defun neotree--human-readable-bytes (bytes)
  "Convert BYTES to human readable format"
  (when (integerp bytes)
    (cl-loop with result = (cons "B" bytes)
             for i in '("k" "M" "G" "T" "P" "E" "Z" "Y")
             while (>= (cdr result) 1024.0)
             do (setf result (cons i (/ (cdr result) 1024.0)))
             finally return
             (pcase (car result)
               ("B" (format "%sb" bytes))
               (_ (format "%.1f%s" (cdr result) (car result)))))))

(defun neotree--get-file-info-message (path)
  "Retrieve file info message for file on given PATH"
  (let ((attr (file-attributes path)))
    (if attr
        (let* ((size (neotree--human-readable-bytes (file-attribute-size attr)))
               (last-modified (format-time-string "%F %T" (file-attribute-modification-time attr)))
               (permissions (file-attribute-modes attr)))
          (format "%s -- %s: %s %s: %s %s: %s"
                  (propertize path 'face 'font-lock-string-face)
                  (propertize "Size" 'face 'font-lock-keyword-face)
                  (propertize size 'face 'font-lock-type-face)
                  (propertize "Last Modified" 'face 'font-lock-keyword-face)
                  (propertize last-modified 'face 'font-lock-type-face)
                  (propertize "Permissions" 'face 'font-lock-keyword-face)
                  (propertize permissions 'face 'font-lock-type-face)))
      "")))

(defun neotree--show-file-info-on-minibuffer ()
  "Display file info on minibuffer"
  (declare-function neo-buffer--get-filename-current-line "neotree")
  (message "%s" (neotree--get-file-info-message (neo-buffer--get-filename-current-line))))

(defun neotree--display-file-info-post-command ()
  "Display file info on post-command hook"
  (when neotree-display-file-info (neotree--show-file-info-on-minibuffer)))

(defun neotree-display-file-info ()
  "Setup display file info for neotree"
  (when (derived-mode-p 'neotree-mode)
    (add-hook 'post-command-hook 'neotree--display-file-info-post-command nil t)))

(provide 'neotree-file-info)
