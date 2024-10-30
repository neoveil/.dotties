(require 'functions)
(require 'tools)

(keymap-global-set "C-s" 'isearch-forward-regexp)
(keymap-global-set "C-r" 'isearch-backward-regexp)
(keymap-global-set "C-M-s" 'isearch-forward)
(keymap-global-set "C-M-r" 'isearch-backward)
(keymap-global-set "M-|" 'align-regexp)
(keymap-global-set "M-/" 'company-complete)
(keymap-global-set "C-c M-/" 'hippie-expand)
(keymap-global-set "C-<return>" 'crux-smart-open-line)
(keymap-global-set "C-S-<return>" 'crux-smart-open-line-above)
(keymap-global-set "C-;" 'comment-line)
(keymap-global-set "C-," 'crux-duplicate-current-line-or-region)
(keymap-global-set "C-c <left>" 'windmove-swap-states-left)
(keymap-global-set "C-c <right>" 'windmove-swap-states-right)
(keymap-global-set "C-c <up>" 'windmove-swap-states-up)
(keymap-global-set "C-c <down>" 'windmove-swap-states-down)
(keymap-global-set "C-c k a" 'kill-all-buffers)
(keymap-global-set "C-c k o" 'kill-other-buffers)
(keymap-global-set "C-c f p" 'find-file-at-point)
(keymap-global-set "C-c s c" 'shell-command)
(keymap-global-set "C-c s a" 'async-shell-command)
(keymap-global-set "C-c x c" 'compile)
(keymap-global-set "C-c /" 'query-replace-global)
(keymap-global-set "M-%" 'anzu-query-replace)
(keymap-global-set "C-c b y" 'browse-kill-ring)

(provide 'keymaps)
