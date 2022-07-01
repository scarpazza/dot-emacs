(show-paren-mode 1)
(delete-selection-mode t)
(global-display-line-numbers-mode t)
(global-font-lock-mode t)
(electric-pair-mode 0) ;; if I want to type a paren, I'll type it myself
(helm-mode)

;;(column-number-mode)


(setq
 case-fold-search          nil         ;; search should be case-sensitive by default
 compilation-read-command  nil         ;; no need to prompt for the read command _every_ times
 compilation-scroll-output t           ;; always scroll
 default-directory         "~/"
 doom-theme                'doom-peacock
 frame-resize-pixelwise    t
 fill-column               120
 gc-cons-threshold         10000000
 helm-ff-allow-non-existing-file-at-point t ;; allow entering selections not listed
 helm-ff-file-name-history-use-recentf t
 inhibit-startup-screen    t           ;; No need to see GNU agitprop.
 initial-scratch-message   nil         ;; No need to remind me what a scratch buffer is.
 kill-whole-line           t           ;; Let C-k delete the whole line.
 lexical-binding t
 magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ;; magit always full screen
 mark-even-if-inactive     nil         ;; Fix undo in commands affecting the mark.
 markdown-header-scaling   t
 mouse-drag-copy-region    t           ;; mouse selection copies text to clipboard automatically
 mouse-wheel-follow-mouse  t           ;; wheel scrolls the window you are hovering, even if inactive
 mouse-wheel-scroll-amount '(1 ((shift) . 5)) ;; smooth scrolling by mouse
 require-final-newline     t
 ring-bell-function        'ignore     ;; Never ding at me, ever.
 scroll-error-top-bottom   t           ;; allow scrolling to very top/bottom of file
 show-trailing-whitespace  t
 use-dialog-box            nil         ;; Prompts should go in the minibuffer, not in a GUI.
 use-package-always-ensure t
 custom-safe-themes        t
 help-window-select        t           ;; Focus new help windows when opened
 indent-tabs-mode          nil         ;; Do not use tab characters to indent
 tab-width                 4
 window-combination-resize t           ;; Resize windows proportionally
 )

(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recent-files")



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

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package whitespace
  :ensure nil
  :hook
  (prog-mode . whitespace-mode)
  (text-mode . whitespace-mode)
  :custom
  (whitespace-style '(face empty indentation::space tab trailing)))

(require 'whitespace)
(set-face-background 'whitespace-space "blue" )
(set-face-background 'whitespace-indentation "gray14" )

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")

(require 'helm)
(set-face-attribute 'helm-source-header nil :background "gray30")

(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))
