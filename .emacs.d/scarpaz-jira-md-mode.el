;;
;; jira-md mode
;;
;; scarpaz@scarpaz.com
;;
;; All the files in these repository are released under the GNU General Public License v3.
;; To learn more, see https://www.gnu.org/licenses/gpl-3.0.en.html
;;
;; This is a minor mode that performs the following:
;; - it colorizes user mentions and jira issue numbers
;; - it resolves user mentions via "finger" when you type or move your cursor on them;
;;   It uses a hash table to resolve usernames (and substrings) only once.
;; - it binds "Alt-Enter" to issue lookup via org-jira.
;;
;; It's intended to be consistent with markdown rendering performed by Atlassian products
;; like bitbucket.
;;
;; It auto-enables when editing markdown files.
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

  (setq scarpaz/user-hash-table (make-hash-table :test 'equal))
  (add-hook 'post-command-hook 'scarpaz/cursor-hook)
  )


;; rewrite me with thing-at-point 'symbole
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
    (org-jira-get-issue issue_no)
    )
  )

(defun scarpaz/display-user-mention (username beg end)
  (setq hashlookup (gethash username scarpaz/user-hash-table))
  (if hashlookup
      (message hashlookup)
    (progn
      (message "Resolving user mention: %s ..." username )
      (puthash username (shell-command-to-string (format "finger -m %s" username))
               scarpaz/user-hash-table)
      (message "+%s" (gethash username scarpaz/user-hash-table))))
)


(defun scarpaz/cursor-hook ()
  (interactive)
  (let* (
         ( bounds (bounds-of-thing-at-point 'symbol) )
         ( beg    (car bounds))
         ( end    (cdr bounds))
         )
    (progn
      (when beg
        (when (char-before beg)
          (when (char-equal ?@ (char-before beg))
            (setq str (buffer-substring-no-properties beg end))
            (scarpaz/display-user-mention str beg end)
            ))))
))

(add-hook 'post-command-hook 'scarpaz/cursor-hook)

;;;###autoload
(add-hook 'markdown-mode-hook 'jira-md-mode)

(provide 'jira-md-mode)

