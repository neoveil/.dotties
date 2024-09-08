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

(defun duplicate-line-or-region (direction)
  (interactive "SDirection (above/down): ")
  (let* ((use-region (use-region-p))
         (text (if use-region
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let ((s (thing-at-point 'line t)))
                   (if s (string-remove-suffix "\n" s) ""))))
         (column (- (point) (if use-region (region-beginning) (point-at-bol))))
         (original-point (point)))
    (deactivate-mark)
    (if (eq direction 'above)
        (progn
          (if use-region
              (goto-char (region-beginning))
            (move-beginning-of-line 1))
          (newline)
          (forward-line -1)
          (insert text)
          (goto-char original-point))
      (if use-region
          (goto-char (region-end))
        (move-end-of-line 1))
      (newline)
      (insert text)
      (if use-region
          (goto-char (+ original-point (length text)))
        (move-beginning-of-line 1)
        (forward-line 0)
        (move-to-column column)))))

(defun duplicate-line-or-region-down ()
  (interactive)
  (duplicate-line-or-region 'down))

(defun duplicate-line-or-region-above ()
  (interactive)
  (duplicate-line-or-region 'above))

(defun start-line-down ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun start-line-above ()
  (interactive)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
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
    (call-interactively 'query-replace-regexp)))

(provide 'functions)
