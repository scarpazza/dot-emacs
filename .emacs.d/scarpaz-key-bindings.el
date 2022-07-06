;; I love helm. Not everybody does. In fact, people are usually very polarized about helm.
;; I customized my key bindings around it quite a bit.
;;
;; The great tutorial that inspired most of my choices is:
;; https://tuhdo.github.io/helm-intro.html
;;
;;

(require 'yafolding)


;; Replace existing features with more advanced, helm-based alternatives
(require 'helm)
(require 'helm-swoop)
(require 'helm-config)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-s")     'helm-swoop)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)


;; BEGIN key binding cleanup
;;
;; Remove the original navigational key bindings
;; - they don't make any sense if you have a keyboard with arrows
;; - they free many precious "modifier + single key" combinations
;;
(global-unset-key (kbd "C-f")) ;; move forward    one character - just use arrow key
(global-unset-key (kbd "C-b")) ;; move backward   one character - just use arrow key
(global-unset-key (kbd "C-p")) ;; move up         one line      - just use arrow key
(global-unset-key (kbd "C-n")) ;; move down       one line      - just use arrow key
(global-unset-key (kbd "C-v")) ;; C-v scroll down one page      - just use the PageDown key
(global-unset-key (kbd "M-v")) ;; M-v scroll up   one page      - just use the PageUp   key
(global-unset-key (kbd "M-f")) ;; move forward    one word      - just use M-arrow key
(global-unset-key (kbd "M-b")) ;; move backward   one word      - just use M-arrow key


;; Remove the stupidest and most annoying key binding in the history of computing
;; - I can't imagine anybody ever needing this feature in 2021
(global-unset-key (kbd "C-z")) ;; suspend-frame


;; Remove bindings that I personally never found useful
(global-unset-key (kbd "C-t")) ;; transpose characters
(global-unset-key (kbd "M-t")) ;; transpose words
(global-unset-key (kbd "M-o")) ;; M-o facemenu-mode
(global-unset-key (kbd "M-/")) ;; dabbrev-expand - too important to require two keystrokes - assigning it to TAB below
(global-unset-key [F3] )       ;; start recording macro
(global-unset-key [F4] )       ;; stop  recording macro
;; END key bindings cleanup



;; utility functions
(defun scarpaz/kill-buffer-window ()
  "Kill current buffer if not modified and close its window. Else, just close its window."
  (interactive)
  (when (buffer-modified-p) (kill-this-buffer))
  (delete-window) )

(defun scarpaz/violent-kill-buffer-window ()
  "Kill current buffer, even if modified, and close its window. Don't ask anything."
  (interactive) (set-buffer-modified-p nil) (kill-this-buffer) (delete-window) )

(defun scarpaz/toggle-line-spacing ()
  "Cycle line spacing between 0, +25%, +50%, +100%, +200%.
   On the M122 keyboard, I bind this function to a dedicated key, on which I installed a 'Line Space'
   keycap taken from an old IBM Wheelwriter typewriter."
  (interactive)
  (if line-spacing
      (if (< line-spacing 2)
          (setq line-spacing (* 2 line-spacing))
        (setq line-spacing nil) )
    (setq line-spacing 0.25))
  (redraw-frame (selected-frame)))



;; BEGIN establish my preferred key bindings
;;
;; Attention: If you are a Mac user, most "C-<fn>" are intercepted by MacOS unless you disable them
;; under System Preferences > Keyboards > Shortcuts
;;
(global-set-key (kbd "C-z")     'undo) ;; nowadays C-z is commonly undo
(global-set-key (kbd "C-v")     'yank) ;; nowadays C-v is commonly paste



(global-set-key (kbd "M-o")     'toggle-truncate-lines) ;;

(global-set-key (kbd "M-b")     'minimap-mode) ;;

(global-set-key (kbd "C-f")     'scarpaz/toggle-line-spacing) ;;

(global-set-key (kbd "<f3>" )   'helm-find-files)             ;; Open, like F3  is "load game" in Wolfenstein 3D
(global-unset-key (kbd "C-x C-f") )                           ;; disable C-x C-f. I want to lose that habit.

(global-set-key (kbd "<f4>" )   'read-only-mode)              ;; toggle read only mode, like F4 is "edit" in Norton Commander
(global-set-key (kbd "<f5>" )   'revert-buffer)               ;; like "refresh page" in any web browser
(global-set-key (kbd "<f6>")    'magit-status)                ;; Memory aid: F6 is move/rename in NC. Think "move to repo


(global-set-key (kbd "<f7>" )   'flyspell-buffer)                  ;; In Microsoft Office, F7 traditionally starts the spell checker
(global-set-key (kbd "C-<f7>" ) 'flyspell-correct-word-before-point) ;;

(global-set-key (kbd "<f8>" )   'kill-this-buffer)            ;; F8 is "delete" in Midnight Commander
(global-set-key (kbd "C-<f8>")  'delete-window)               ;; C-F8 is a stronger version of F8

(global-set-key (kbd "<f9>" )   'scarpaz/safe-delete-trailing-whitespace)  ;;
(global-set-key (kbd "<f12>")   'other-window)                ;;


(global-set-key (kbd "M-+")    'magit-stage-file)        ;; "Alt +" stages the file on which you are working
(global-set-key (kbd "M--")    'magit-unstage-file)      ;; "Alt -" unstages the file on which you are working

(global-set-key (kbd "M-/")           'helm-semantic-or-imenu)  ;; "Alt/" brings up the semantic menu


(define-key lisp-mode-map (kbd "M-<return>")       'eval-last-sexp) ;; in lisp, "act" means evaluate the subexpr
(define-key emacs-lisp-mode-map (kbd "M-<return>") 'eval-last-sexp) ;;



(define-key helm-read-file-map  (kbd "<left>")  'backward-char)
(define-key helm-read-file-map  (kbd "<right>") 'forward-char)
(define-key helm-find-files-map (kbd "<left>")  'backward-char)
(define-key helm-find-files-map (kbd "<right>") 'forward-char)


;; Set up a keybindings to get rid of buffers expeditiously
;; MacOS: DEL corresponds to the key labeled "delete" on macs, which is really a backspace.
;; On PCs

(define-key helm-buffer-map (kbd "DEL") 'helm-buffer-run-kill-persistent)
(define-key helm-buffer-map (kbd "<backspace>") 'helm-buffer-run-kill-persistent)
(define-key helm-buffer-map (kbd "<deletechar>") 'helm-buffer-run-kill-persistent)



(defun scarpaz/safe-delete-trailing-whitespace ()
  "Fixes an incompatibility between yafolding-mode and delete-trailing-whitespace.
   Unfolds all items before deleting trailing whitespace"
  (interactive)
  (when (bound-and-true-p yafolding-mode)
        (yafolding-show-all))
  (delete-trailing-whitespace)
  )


;; Tab completion
;; https://www.emacswiki.org/emacs/TabCompletion

(defun fancy-tab (arg)
  (interactive "P")
  (setq this-command last-command)
  (if (or (eq this-command 'hippie-expand) (looking-at "\\_>"))
      (progn (setq this-command 'hippie-expand)
	     (hippie-expand arg))
    (setq this-command 'indent-for-tab-command)
    (indent-for-tab-command arg)))

(define-key read-expression-map [(tab)] 'hippie-expand)
(global-set-key (kbd "TAB") 'fancy-tab)

;; END establish my preferred key bindings
