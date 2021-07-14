; most material shamelessly stolen from https://github.com/patrickt/emacs

(setq lexical-binding t)
(setq gc-cons-threshold 10000000)

(show-paren-mode 1)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(setq
   custom-safe-themes t
   use-package-always-ensure t
   ;; No need to see GNU agitprop.
   inhibit-startup-screen t
   ;; No need to remind me what a scratch buffer is.
   initial-scratch-message nil
   ;; Never ding at me, ever.
   ring-bell-function 'ignore
   ;; Prompts should go in the minibuffer, not in a GUI.
   use-dialog-box nil
   ;; Fix undo in commands affecting the mark.
   mark-even-if-inactive nil
   ;; Let C-k delete the whole line.
   kill-whole-line t
   ;; search should be case-sensitive by default
   case-fold-search nil
   ;; no need to prompt for the read command _every_ time
   compilation-read-command nil
   ;; always scroll
   compilation-scroll-output t
   ;; my source directory
   default-directory "~/"
   )

(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)

(setq doom-theme 'wheatgrass)

(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))


;; USER LOOKUP UTILITIES
;; (intended to check user mentions in bitbucket markdown documents as you type them)
;; ... unfinished
(defun scarpaz/lookup-user ()
  "Interpret the current word as a username (ignoring any @ prefixes) and finger that user."
  (interactive)
  (let ((username (replace-regexp-in-string "@" "" (thing-at-point 'word) )))
    (message "%s = %s" username
	     (shell-command-to-string (format "finger %s" username)
	   ))))


;; hook a finger lookup to every word starting with '@'
(defun scarpaz/lookup-if-user (begin end length)
  "If current word starts with @, look it up as a user."
  (interactive)
  (let ( ( username   (thing-at-point 'word))
	 ( beforeword (char-before (car (bounds-of-thing-at-point 'word ))))
	 )
    (when (and (eq ?@ beforeword) (> (length username) 2)) 
      (message "%s = %s" username
	       (shell-command-to-string (format "finger %s" username)
					)))))
(add-hook 'after-change-functions 'scarpaz/lookup-if-user nil t)


;; SAVE-TIME TWEAKS
;;
;; When I'm working on text (fundamental mode or markdown) 
;; I want flyspell squiggles to come up every time I save.
;; All I need to do to see the squiggles is save the file.
;; If the squiggles bother me, all I need to do to make them disappear is to attempt to save an unmodified (just saved) file.
;;

(defun scarpaz-flyspell-off () (interactive) (flyspell-mode 0))

(defun scarpaz-flyspell-before-saving ()
  (when (memq major-mode '(fundamental-mode markdown-mode)) (flyspell-buffer)))

(defun scarpaz-save-buffer (&optional arg) (interactive) 
  (if (buffer-modified-p) (save-buffer arg) (flyspell-mode 0)) ) 

(add-hook 'before-save-hook #'scarpaz-flyspell-before-saving)

(global-set-key [f2] 'scarpaz-save-buffer)             ; F2 is safe, like in Wolfenstein 3D
(global-set-key (kbd "C-x C-s") 'scarpaz-save-buffer ) ; remap save-buffer keybinding to my custom save
(global-set-key (kbd "C-<f2>" ) 'scarpaz-flyspell-off) ; ^F2 forces flyspell off. 

;; END OF SAVE-TIME TWEAKS
;;

(global-set-key [f8] 'kill-buffer)  # F8 is kill buffer, like "delete" in Norton Commander


;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(global-unset-key (kbd "C-z")) ;; suspend-frame
(global-unset-key (kbd "M-o")) ;; facemenu-mode

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq require-final-newline t)

(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(use-package doom-themes
  :config
  (let ((chosen-theme 'wheatgrass))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
    (load-theme chosen-theme)))

(electric-pair-mode)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))


(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" default))
 '(package-selected-packages
   '(which-key neotree doom-modeline doom magit markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
