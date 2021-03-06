;;
;; Sample key bindings for a retrofitted IBM M122 "battle cruiser" keyboard,
;; as discussed in:
;;
;;       https://github.com/scarpazza/battlecruiser
;;
;; For ease of testing, and in an attempt to make experience close enough whenever I don't have an
;; M122 keyboard, I'm binding the same function associated each key in F13 ... F24 also to combination
;; Control-F<n-12>, i.e., F13 does the same thing as ^F1, F14 does the same as ^F2, and so on.)
;;
;; This file *only* covers key bindings associated with the exotic extra keys of the IBM M122.
;; My normal keybindings are in https://github.com/scarpazza/dot-emacs/blob/main/.emacs.d/scarpaz-key-bindings.el
;; and their discussion is at https://github.com/scarpazza/dot-emacs#key-bindings

(require 'jira-md-mode)

(defun scarpaz/unassigned ()
  (interactive) (message "Unassigned battlecruiser key code") )

;; TO DO - this function gets confused by minimaps
;;
(defun scarpaz/transpose-windows (arg)
  "Transpose the buffers shown in two windows. - https://www.emacswiki.org/emacs/TransposeWindows"
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


(defun scarpaz/magit-toggle-blame (arg)
  (interactive "p")
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-quit)
      (magit-blame-addition nil)
      )
  )



(global-set-key (kbd "C-, z" )  'dabbrev-expand)         ;; Key between LSHIFT and Z - use autocompletion

(global-set-key (kbd "C-, 9" )  'calendar)               ;; Blank NumKeypad key, rightmost column, second row - toggle calendar window
(eval-after-load "calendar"
  `(progn
     (define-key calendar-mode-map (kbd "C-, 9") 'calendar-exit)))

(global-set-key (kbd "C-< 9" ) 'scarpaz/insert-today)    ;; Shift + same key above - insert today's date


;; EXTRA_F1  is not available for binding - I remap it to Esc
;; EXTRA_F10 is not available for binding - I remap it to Hyper/Windows/GUI


(global-set-key (kbd "C-, 1" )  'yafolding-toggle-all)           ;; EXTRA_F2
(global-set-key (kbd "C-< 1" )  'yafolding-hide-all)             ;; Shift+EXTRA_F2

;; (global-set-key (kbd "C-, 1" )  'keyboard-escape-quit)           ;; EXTRA_F2 - i no longer like this
(global-set-key (kbd "C-, 2" )  'kill-rectangle)                 ;; EXTRA_F3
(global-set-key (kbd "C-, 3" )  'yank-rectangle)                 ;; EXTRA_F4

(global-set-key (kbd "C-, 4" ) 'markdown-promote)                ;; EXTRA_F5
(global-set-key (kbd "C-, 5" ) 'markdown-demote)                 ;; EXTRA_F6

(global-set-key (kbd "C-, 6" ) 'display-line-numbers-mode)       ;; EXTRA_F7
(global-set-key (kbd "C-< 6" ) 'whitespace-mode)                 ;; Shift + EXTRA_F7
(global-set-key (kbd "C-, 7" ) 'comment-dwim)                    ;; EXTRA_F8
(global-set-key (kbd "C-< 7" ) 'uncomment-region)                ;; Shift + EXTRA_F8
(global-set-key (kbd "C-, 8" ) 'scarpaz/toggle-line-spacing)     ;; EXTRA_F9

;; "Rule" key, i.e., the key in the middle of the arrow keys
(global-set-key (kbd "C-, 0" )  'scarpaz/act-on-element)         ;; "Rule"       -> expand region

;;(global-set-key (kbd "C-, 0" )  'er/expand-region)                   ;; "Rule"       -> expand region
;;(global-set-key (kbd "C-< 0" )  'er/contract-region)                 ;; Shift-"RULE" -> contract region


;; F13
(global-set-key (kbd "C-, a" )  'magit-wip-log-current)   ;; memory aid: "HELP me understand what's going on": it's on top of F1 - help
(global-set-key (kbd "C-<f1>")  'magit-wip-log-current)   ;; ^F1 duplicates F13

;; F14
(global-set-key (kbd "C-, b" )  'magit-stage-file)       ;; memory aid: it's a stronger for of SAVE; it's on top of F2 (save)
(global-set-key (kbd "C-<f2>")  'magit-stage-file)       ;; ^F2 duplicates F14

;; F15
(global-set-key (kbd "C-, c" )  'helm-ag) ;; bring up the helm interface to ag "the silver searcher"; it's a broader form of load; F3 is load
(global-set-key (kbd "C-<f3>" ) 'helm-ag)

;; F16
(global-set-key (kbd "C-, d" )  'magit-diff-buffer-file)      ;; memory aid "show me the edits" is right above F4 (toggle edit)
(global-set-key (kbd "C-<f4>")  'magit-diff-buffer-file)      ;; ^F4 duplicates F16

;; F17
(global-set-key (kbd "C-, e" )  'scarpaz/magit-toggle-blame)  ;; "Show me the refreshes" - on top of F5 = refresh
(global-set-key (kbd "C-<f5>" ) 'scarpaz/magit-toggle-blame)  ;; ^F5 duplicates F17

;; F18
(global-set-key (kbd "C-, f" )  'eshell)                      ;;
(global-set-key (kbd "C-<f6>" ) 'eshell)                      ;; ^F6 duplicates F18

;; F19
(global-set-key (kbd "C-, g" )  'flyspell-correct-word-before-point) ;; memory aid: it's just above F7, that does spelling
(global-set-key (kbd "C-<f7>" ) 'flyspell-correct-word-before-point) ;; ^F7 duplicates F19

;; F20
(global-set-key (kbd "C-, h" )  'delete-window)             ;; memory aid: it's similar to F8. F8 deletes the buffer. F20 deletes the window.
(global-set-key (kbd "C-<f8>")  'delete-window)             ;; ^F8 duplicates F20

;; F21
(global-set-key (kbd "C-, i" )  'previous-buffer)           ;; memory aid - on the M122, the F9 key says "Prev" on its root
(global-set-key (kbd "C-<f9>")  'previous-buffer)           ;; ^F9 duplicates F21

;; F22
(global-set-key (kbd "C-, j" )  'next-buffer)               ;; memory aid - on the M122, the F10 key says "Next" on its root
(global-set-key (kbd "C-<f10>") 'next-buffer)               ;; ^F10 duplicates F22

;; F23
(global-set-key (kbd "C-, k" )  'delete-other-windows)      ;; memory aid - spacing is a focus aid like fullscreen is
(global-set-key (kbd "C-<f11>") 'delete-other-windows)      ;; ^F11 duplicates F23

;; F24
(global-set-key (kbd "C-, l" )  'helm-mini) ;; memory aid: on top of F12 that does "other window"
(global-set-key (kbd "C-<f12>") 'helm-mini) ;; ^F12 duplicates F24
