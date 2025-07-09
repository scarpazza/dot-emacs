(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(show-paren-mode 1)
(delete-selection-mode t)
(global-display-line-numbers-mode 0)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(global-font-lock-mode t)
(electric-pair-mode 0) ;; if I want to type a paren, I'll type it myself

;;(column-number-mode)



(setq
 case-fold-search              nil          ;; search should be case-sensitive by default
 company-idle-delay            0.0
 company-minimum-prefix-length 1
 compilation-read-command      nil          ;; no need to prompt for the read command _every_ times
 compilation-scroll-output     t            ;; always scroll
 custom-safe-themes            t
 default-directory             "~/"
 doom-theme                    'doom-peacock
 fill-column                   120
 frame-resize-pixelwise        t
 gc-cons-threshold             (* 100 1024 1024)
 helm-ff-allow-non-existing-file-at-point t ;; allow entering selections not listed
 helm-ff-file-name-history-use-recentf t
 help-window-select            t           ;; Focus new help windows when opened
 indent-tabs-mode              nil         ;; Do not use tab characters to indent
 inhibit-startup-screen        t           ;; No need to see GNU agitprop.
 initial-scratch-message       nil         ;; No need to remind me what a scratch buffer is.
 kill-whole-line               t           ;; Let C-k delete the whole line.
 lexical-binding               t
 lsp-idle-delay                0.1         ;; clangd is fast
 magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1 ;; magit always full screen
 mark-even-if-inactive         nil         ;; Fix undo in commands affecting the mark.
 markdown-header-scaling       t
 mouse-drag-copy-region        t           ;; mouse selection copies text to clipboard automatically
 mouse-wheel-follow-mouse      t           ;; wheel scrolls the window you are hovering, even if inactive
 mouse-wheel-scroll-amount     '(1 ((shift) . 5)) ;; smooth scrolling by mouse
 org-agenda-files              '("~/org")
 org-support-shift-select      t           ;; I want shift-arrows for selection
 read-process-output-max       (* 1024 1024)
 require-final-newline         t
 ring-bell-function            'ignore     ;; Never ding at me, ever.
 scroll-error-top-bottom       t           ;; allow scrolling to very top/bottom of file
 show-trailing-whitespace      t
 tab-width                     4
 treemacs-space-between-root-nodes nil
 use-dialog-box                nil         ;; Prompts should go in the minibuffer, not in a GUI.
 use-package-always-ensure     t
 window-combination-resize     t           ;; Resize windows proportionally
 )


(use-package rainbow-mode  :ensure t)

(use-package all-the-icons :ensure t)
(use-package avy           :ensure t)
(use-package company       :ensure t)
(use-package dap-mode      :ensure t)
(use-package doom-themes   :ensure t)
(use-package efar          :ensure t)
(use-package expand-region :ensure t)
(use-package flycheck      :ensure t)
(use-package helm          :ensure t)
;(use-package helm-files    :ensure t) -- no longer available -- investigate
;(use-package helm-ag       :ensure t) -- no longer available -- investigate
(use-package helm-core     :ensure t)
(use-package helm-lsp      :ensure t)
(use-package helm-xref     :ensure t)
(use-package hydra         :ensure t)
(use-package lsp-mode      :ensure t)
(use-package lsp-treemacs  :ensure t)
(use-package magit         :ensure t)
(use-package org-jira      :ensure t)
(use-package org-modern    :ensure t)
(use-package ox-gfm        :ensure t)
(use-package projectile    :ensure t)
(use-package swiper-helm   :ensure t)
(use-package which-key     :ensure t)
(use-package yafolding     :ensure t)
(use-package yasnippet     :ensure t)

(helm-mode)

(use-package recentf
  :ensure t
  :init
  (setq
    recentf-save-file "~/.emacs.d/recentf"
    recentf-max-saved-items 10000
    recentf-max-menu-items 5000
    )
  (recentf-mode 1)
  (run-at-time nil (* 5 60) 'recentf-save-list)
)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package all-the-icons-dired
   :ensure t
   :after all-the-icons
   :hook (dired-mode . all-the-icons-dired-mode))


(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(require 'expand-region) ;; needed for byte compilation

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(require 'magit) ;; needed for byte compilation

(use-package whitespace
  :ensure t
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
(custom-set-variables
'(helm-follow-mode-persistent t))

(custom-set-variables
  '(helm-ag-command-option "--ignore-case")
  '(helm-ag-use-agignore t))

;; do not prepopulate swoop search term with the one at point
(setq helm-swoop-pre-input-function (lambda () nil))

(use-package which-key
  :ensure t
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))


(setq org-todo-keywords
  '((sequence "BACKLOG" "TODO" "IN_PROGRESS" "IN_REVIEW" "WAITING" "DONE" "DECLINED")))

(add-hook 'org-mode-hook 'org-modern-mode)

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  )

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))
