;; -*- lexical-binding: t; -*-

(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (remq (current-buffer) (buffer-list))))

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun query-replace-global ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (call-interactively 'query-replace-regexp)))

(defmacro put-all (&rest args)
  `(progn
     ,@(let ((r nil) (xs args))
         (while xs
           (push `(put ',(pop xs) ',(pop xs) ,(pop xs)) r))
         r)))

(provide 'functions)
