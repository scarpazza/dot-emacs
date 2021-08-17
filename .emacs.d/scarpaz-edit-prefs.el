(show-paren-mode 1)
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(global-font-lock-mode t)
(electric-pair-mode 0) ;; if I want to type a paren, I'll type it myself
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
 frame-resize-pixelwise    t
 kill-whole-line           t           ;; Let C-k delete the whole line.

 case-fold-search          nil         ;; search should be case-sensitive by default
 compilation-read-command  nil         ;; no need to prompt for the read command _every_ times
 compilation-scroll-output t           ;; always scroll

 mouse-drag-copy-region    t           ;; mouse selection copies text to clipboard automatically
 mouse-wheel-scroll-amount '(1 ((shift) . 5)) ;; smooth scrolling by mouse
 mouse-wheel-follow-mouse  t           ;; wheel scrolls the window you are hovering, even if inactive

 default-directory         "~/"
 doom-theme                'doom-peacock
 require-final-newline     t
 show-trailing-whitespace  t
 scroll-error-top-bottom   t           ;; allow scrolling to very top/bottom of file

 magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ;; magit always full screen
 gc-cons-threshold         10000000

 helm-ff-allow-non-existing-file-at-point t ;; allow entering selections not listed
 markdown-header-scaling   t
 )



(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))


(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


(use-package doom-modeline
  :config (doom-modeline-mode))

(require 'expand-region)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))



(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")


(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

