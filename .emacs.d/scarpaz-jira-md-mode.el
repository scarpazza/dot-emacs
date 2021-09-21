;;
;; jira-md mode
;;
;; This is a minor mode that performs the following:
;; - it colorizes user mentions and jira issue numbers
;; - it binds "Alt-Enter" to issue lookup via org-jira.
;;
;; It's intended to be consistent with markdown rendering performed by Atlassian products
;; like bitbucket.
;;
;; It auto-enables when editing markdown.
;;


(require 'font-lock)
(require 'org-jira)



(setq jira-md-keywords
      '(("[A-Z]+-[0-9]+" . font-lock-function-name-face)
        ("@[a-z0-9_]+"    . font-lock-constant-face))
      )


;;;###autoload
(define-minor-mode jira-md-mode
  "Get your foos in the right places."
  :lighter " jira-md"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-<return>") 'scarpaz/go-jira)
            map)

  (font-lock-add-keywords nil jira-md-keywords)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))
    )
)

(defun scarpaz/go-jira ()
  (interactive)
  (let* (
         (initialpt (point))
         (ignore    (skip-chars-backward "A-Z0-9-"))
         (start     (point))
         (ignore    (skip-chars-forward "A-Z0-9-"))
         (end       (point))
         (issue_no  (buffer-substring-no-properties start end))
         )
    (goto-char initialpt)
    (message "Requesting jira issue %s" issue_no)
    (org-jira-get-issue issue_no)
    )
 )


;;;###autoload
(add-hook 'markdown-mode-hook 'jira-md-mode)

(provide 'jira-md-mode)

;; HPC-123




