(show-paren-mode 1)
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(electric-pair-mode)
(helm-mode)

;;(column-number-mode)

(setq
 custom-safe-themes        t
 lexical-binding t

 use-package-always-ensure t
 inhibit-startup-screen    t           ;; No need to see GNU agitprop.
 initial-scratch-message   nil         ;; No need to remind me what a scratch buffer is.
 ring-bell-function        'ignore     ;; Never ding at me, ever.
 use-dialog-box            nil         ;; Prompts should go in the minibuffer, not in a GUI.
 mark-even-if-inactive     nil         ;; Fix undo in commands affecting the mark.
 kill-whole-line           t           ;; Let C-k delete the whole line.

 case-fold-search          nil         ;; search should be case-sensitive by default
 compilation-read-command  nil         ;; no need to prompt for the read command _every_ times
 compilation-scroll-output t           ;; always scroll
 default-directory         "~/"
 doom-theme                'doom-peacock
 require-final-newline     t
 show-trailing-whitespace  t
 magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ;; magit always full screen
 gc-cons-threshold         10000000
 )


(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  )
