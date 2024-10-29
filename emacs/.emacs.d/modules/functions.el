(defun put-buffer-name-on-clipboard ()
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))

(defun put-file-name-on-clipboard ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))

(defun load-path-here ()
  (interactive)
  (add-to-list 'load-path default-directory))

(defun duplicate-line ()
  (interactive)
  (let ((column (- (point) (line-beginning-position)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(defun start-line-down ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun start-line-above ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (forward-line -1)
  (indent-for-tab-command))

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun query-replace-global ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'anzu-query-replace-regexp)))

(provide 'functions)
