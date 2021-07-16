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
(global-set-key (kbd "C-<f2>" ) 'scarpaz-flyspell-off) ; ^F2 forces flyspell off.

;; END OF SAVE-TIME TWEAKS
;;
