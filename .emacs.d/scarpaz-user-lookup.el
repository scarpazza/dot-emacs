;; USER LOOKUP UTILITIES
;; (intended to check user mentions in bitbucket markdown documents as you type them)
;; ... unfinished


(defun scarpaz/lookup-user-command ()
  "Interactive command - interpret the current word as a username (ignoring any @ prefixes) and finger that user. You can bind this function to a key."
  (interactive)
  (let ((username (replace-regexp-in-string "@" "" (thing-at-point 'word) )))
    (message "%s = %s" username
	     (shell-command-to-string (format "finger %s" username)
	   ))))

;; edit hook
;; performs an OS "finger" lookup every time you edit a word that starts with '@'
(defun scarpaz/lookup-if-user (begin end length)
  "If current word starts with @, look it up as a user."
  (interactive)
  (let ( ( username   (thing-at-point 'word))
	 ( beforeword (char-before (car (bounds-of-thing-at-point 'word ))))
	 )
    (when (and (eq ?@ beforeword) (> (length username) 2)) 
      (message "%s = %s" username
	       (shell-command-to-string (format "finger %s" username)
					)
	       )
      )
    )
  )

;; major mode change hook
;; installs the edit hook just above ONLY if the buffer enters markdown-mode
(defun scarpaz/mode-change-hook (&rest args)
  "Major mode changes are a convenient place to install local hooks."
  (interactive)
  (message "buffer %s - %s" buffer-file-name major-mode)
  (when (eq major-mode 'markdown-mode)
    (add-hook 'after-change-functions 'scarpaz/lookup-if-user nil t)
    (message "Installed user lookup hook to buffer %s - %s" buffer-file-name major-mode)
    )
  )

(add-hook 'after-change-major-mode-hook 'scarpaz/mode-change-hook nil nil)

(message "DONE loading scarpaz-user-lookup.el")
