;; -*- lexical-binding: t; -*-

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
