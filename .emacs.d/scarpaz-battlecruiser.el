;;
;; Sample key bindings for a retrofitted IBM M122 "battle cruiser" keyboard,
;; as discussed in:
;;       https://github.com/scarpazza/battlecruiser
;; 

(defun scarpaz/unassigned ()
  (interactive) (message "Unassigned battlecruiser key code") )


(global-set-key (kbd "C-, 1" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 2" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 3" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 4" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 5" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 6" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 7" )  'scarpaz/unassigned) ;; EXTRA_F2
(global-set-key (kbd "C-, 8" )  'scarpaz/unassigned) ;; EXTRA_F2

(global-set-key (kbd "C-, 9" )  'scarpaz/unassigned) ;; "RULE"

(global-set-key (kbd "C-, 0" )  'scarpaz/unassigned) ;; NUM_LOCK


(global-set-key (kbd "C-, a" )  'scarpaz/unassigned) ;; F13
(global-set-key (kbd "C-, b" )  'scarpaz/unassigned) ;; F14
(global-set-key (kbd "C-, c" )  'scarpaz/unassigned) ;; F15
(global-set-key (kbd "C-, d" )  'scarpaz/unassigned) ;; F16
(global-set-key (kbd "C-, e" )  'scarpaz/unassigned) ;; F17
(global-set-key (kbd "C-, f" )  'scarpaz/unassigned) ;; F18
(global-set-key (kbd "C-, g" )  'scarpaz/unassigned) ;; F19
(global-set-key (kbd "C-, h" )  'scarpaz/unassigned) ;; F20
(global-set-key (kbd "C-, i" )  'scarpaz/unassigned) ;; F21
(global-set-key (kbd "C-, j" )  'scarpaz/unassigned) ;; F22
(global-set-key (kbd "C-, k" )  'scarpaz/unassigned) ;; F23
(global-set-key (kbd "C-, l" )  'scarpaz/unassigned) ;; F24

(global-set-key (kbd "C-, x" )  'scarpaz/unassigned) ;; Key on the immediate right of LSHIFT
(global-set-key (kbd "C-, y" )  'scarpaz/unassigned) ;; Keypad rightmost column, second row 
(global-set-key (kbd "C-, z" )  'scarpaz/unassigned) ;; Keypad rightmost column, third row 
