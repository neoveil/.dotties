;;; functions.el -*- lexical-binding: t; -*-

(defmacro advice-add-all (how f &rest syms)
  "Add advice F to multiple functions SYMS using HOW.

HOW is the advice type (e.g. :before, :after, :around).
F is the advising function.

SYMS is a list of function symbols. For each symbol in SYMS,
this macro expands into an `advice-add' call that adds F as
advice with the given HOW.

Example:
  (advice-add-all :before #`my-log'
    find-file
    save-buffer
    write-file)

expands into:
  (progn
    (advice-add #`find-file' :before #`my-log')
    (advice-add #`save-buffer' :before #`my-log')
    (advice-add #`write-file' :before #`my-log'))"
  (declare (indent defun))
  `(progn
     ,@(mapcar
        (lambda (sym) `(advice-add #',sym ,how ,f))
        syms)))

(defmacro advice-remove-all (&rest syms)
  "Remove all advices from each function in SYMS.

SYMS is a list of function symbols. For each symbol, every piece
of advice currently attached to that function is removed. This is
implemented by iterating over all advice identifiers returned by
`advice-mapc' and calling `advice-remove' on each one.

Example:
  (advice-remove-all find-file save-buffer)

expands into:
  (progn
    (advice-mapc (lambda (a _) (advice-remove #`find-file' a))
                 #`find-file')
    (advice-mapc (lambda (a _) (advice-remove #`save-buffer' a))
                 #`save-buffer'))"
  `(progn
     ,@(mapcar
        (lambda (sym)
          `(advice-mapc
            (lambda (a _)
              (advice-remove #',sym a))
            #',sym))
        syms)))

(defmacro put-all (&rest args)
  "Set multiple symbol properties in a compact form.

ARGS is interpreted in groups of three elements:
  SYMBOL PROPERTY VALUE

Each triplet expands into:
  (put `SYMBOL' `PROPERTY' VALUE)

The macro collects all triplets and wraps them in a single PROGN.

Example:
  (put-all
    foo bar 10
    baz qux \"hello\")

expands into:
  (progn
    (put `foo' `bar' 10)
    (put `baz' `qux' \"hello\"))"
  `(progn
     ,@(let ((r nil) (xs args))
         (while xs
           (push `(put ',(pop xs) ',(pop xs) ,(pop xs)) r))
         r)))

(defmacro eglot--ensure-all (modes)
  "Add `eglot-ensure' for each symbol in MODES (a quoted list).

Each element in MODES should be a symbol without “-mode”."
  `(progn
     ,@(mapcar
        (lambda (m)
          `(add-hook ',(intern (format "%s-mode-hook" m))
                     #'eglot-ensure))
        (cadr modes))))

(defun all-the-icons--install-fonts-if-not-installed ()
  "Install `all-the-icons' fonts if they are not yet installed, else no-op"
  (defvar all-the-icons-fonts-subdirectory)
  (declare-function all-the-icons-install-fonts nil)
  (unless (file-exists-p
           (file-name-concat
			      (or (getenv "XDG_DATA_HOME")
                (file-name-concat (getenv "HOME") ".local" "share"))
			      "fonts"
			      all-the-icons-fonts-subdirectory
			      "all-the-icons.ttf"))
    (message "`all-the-icons' fonts are not installed, downloading and installing it...")
    (all-the-icons-install-fonts t)))

(defun wdired--restore-hl-line-advice (&rest _)
  "Enable `hl-line-mode'.

To be used as :after advice to `wdired-finish-edit' and `wdired-abort-changes'"
  (hl-line-mode 1))

(defun wdired--register-restore-hl-line-mode-on-exit ()
  "Register advices to restore `hl-line-mode' upon `wdired' exit"
  (declare-function wdired-finish-edit nil)
  (declare-function wdired-abort-changes nil)
  (advice-add-all :after #'wdired--restore-hl-line-advice
    wdired-finish-edit
    wdired-abort-changes))

(defun term--better-exit-handler-advice (f &optional proc msg)
  "Better handler for `term' exit.

It should kill the buffer associated with `term' upon exit.

It also suppresses the default `term' exit message,
substituting it for a better one.

The message should look like \"PROC\" when there is no MSG,
else \"PROC | MSG\".

To be used as :around advice to `term-handle-exit'"
  (let ((inhibit-message t))
    (funcall f proc msg))
  (message "%s%s" proc (concat
                        (when msg " | ")
                        (string-trim (or msg ""))))
  (kill-buffer (current-buffer)))

(defun xterm-color--colorize-compilation-advice (f p s)
  "Colorize compilation output using `xterm-color'.

Applies `xterm-color-filter' to text `s'.

To be used as :around advice to `compilation-filter'"
  (declare-function xterm-color-filter nil)
  (funcall f p (xterm-color-filter s)))

(defun set-local-tab-width ()
  "Set `tab-width' to 2 and `indent-tabs-mode' to nil locally"
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil))

(defun enable-c-line-comment-style ()
  "Enable line comment style for `cc-mode'"
  (declare-function c-toggle-comment-style nil)
  (c-toggle-comment-style -1))

(defun enable-word-wrap ()
  "Enable word wrap"
  (toggle-word-wrap 1))

(defun disable-hl-line-mode ()
  "Disable `hl-line-mode'"
  (hl-line-mode -1))

(defun disable-display-line-numbers-mode ()
  "Disable `display-line-numbers-mode'"
  (display-line-numbers-mode -1))

(defun quit-window-kill-buffer ()
  "Quit the current window killing its buffer"
  (interactive)
  (quit-window t))

(defun kill-other-buffers ()
  "Kill all buffers except the current one"
  (interactive)
  (mapc #'kill-buffer (remq (current-buffer) (buffer-list))))

(defun kill-all-buffers ()
  "Kill all buffers"
  (interactive)
  (mapc #'kill-buffer (buffer-list)))

(defun query-replace-global ()
  "Go to beginning of file and call `query-replace-regexp'"
  (interactive)
  (declare-function anzu-query-replace-regexp nil)
  (save-excursion
    (goto-char (point-min))
    (call-interactively #'anzu-query-replace-regexp)))

(defun term-other-window ()
  "Open another window and call `term' on it"
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*terminal*"))
  (call-interactively #'term))

(defun go-home ()
  "Call `cd' with DIR \"~\""
  (interactive)
  (cd "~"))

(defun make-temp-window (name mode)
  "Show a temporary window using MODE.

MODE is a function (symbol) naming a major mode, e.g. `fundamental-mode'
or `literal-calc-mode'.

NAME is the buffer name.

The buffer is made writable and gets local bindings:
  C-c C-q  -> `quit-window'
  C-c C-k  -> `quit-window-kill-buffer'"
  (let* ((buf (get-buffer name)))
    (if buf
        (pop-to-buffer buf)
      (with-current-buffer-window name nil nil
        (funcall mode))
      (with-current-buffer name
        (setq buffer-read-only nil)
        (let ((keymap (make-sparse-keymap)))
          (set-keymap-parent keymap (current-local-map))
          (define-key keymap (kbd "C-c C-q") #'quit-window)
          (define-key keymap (kbd "C-c C-k") #'quit-window-kill-buffer)
          (use-local-map keymap))))
    (when-let ((win (get-buffer-window name)))
      (select-window win))))

(defun open-temp-buffer ()
  "Open the temporary buffer"
  (interactive)
  (make-temp-window "*temp*" #'fundamental-mode))

(defun open-literate-calc-temp-buffer ()
  "Open a temporary buffer with `literate-calc-mode'"
  (interactive)
  (declare-function literate-calc-mode nil)
  (make-temp-window "*calc*" #'literate-calc-mode))

(provide 'functions)
