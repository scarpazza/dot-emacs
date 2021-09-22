;;
;; jira-md mode
;;
;; Daniele P. Scarpazza - scarpaz@scarpaz.com
;;
;; This is a minor mode that performs the following:
;; - it colorizes
;;   - user mentions (e.g., @user)
;;   - jira issue numbers (e.g., PROJECT-1234)
;;   - dates, either in ISO or US format (e.g., 2010-12-23 or 1997/12/31)
;; - it resolves user mentions via "finger" when you type or move your cursor on them;
;;   It uses a hash table to resolve usernames (and substrings) only once.
;; - it binds "Alt-Enter" to issue lookup via org-jira.
;;
;; I don't recognize European style dates because they can't be distinguished from the
;; American-style ones just by syntax.
;;
;; It's intended to be consistent with markdown rendering performed by Atlassian products like
;; bitbucket.
;;
;; It auto-enables when editing markdown files.
;;
;; All files in this repository are released under the GNU General Public License v3.  To learn
;; more, see https://www.gnu.org/licenses/gpl-3.0.en.html
;;


(require 'font-lock)
(require 'org-jira)

(provide 'jira-md-mode)


;; Customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup jira-md nil
  "Atlassian-oriented markdown integration, including user mentions and issue ."
  :prefix "jira-md/"
  :group 'jira-md
  :link "https://github.com/scarpazza/dot-emacs"
  )

(defcustom jira-md/issue-id-regex
  "[A-Z][A-Z0-9]+-[0-9]+"
  "Regular expression determining what tokens are recognized as Jira issue IDs. By default, all strings of capitals and digits starting with a capital, followed by a dash, followed by digits only. Replace the portion before the dash with <PROJECT> if you want jira-md to only recognize issues from <PROJECT>."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

(defcustom jira-md/user-id-regex
  "@[a-zA-Z0-9_-]+"
  "Regular expression determining what tokens are recognized as user IDs. By default, all strings starting with an at-sign, followed by alphanumeric digits or underscores. Don't change it unless you know what you are doing."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

(defcustom jira-md/iso-date-regex
  "[1-2][0-9]\\{3\\}[-/]\\(1[0-2]\\|0?[1-9]\\)[-/]\\([0-3][0-9]\\|[1-9]\\)"
  "Regular expression determining what tokens are recognized as ISO dates, e.g., YYYY-MM-DD. Change this if you need to represent dates in other millennia than the current one."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

(defcustom jira-md/us-date-regex
  "\\(1[0-2]\\|0?[1-9]\\)[-/]\\([0-3][0-9]\\|[1-9]\\)[-/][1-2][0-9]\\{3\\}"
  "Regular expression determining what tokens are recognized as US dates, e.g., MM-DD-YYYY. Change this if you need to represent dates in other millennia than the current one."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(setq jira-md-keywords
      '(( jira-md/issue-id-regex . font-lock-function-name-face) ;; jira issues
        ( jira-md/user-id-regex  . font-lock-constant-face)      ;; user mentions
        ( jira-md/iso-date-regex . font-lock-keyword-face)       ;; dates
        ( jira-md/us-date-regex  . font-lock-keyword-face)       ;; dates
        )
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
         (ignore    (skip-chars-backward "A-Za-z0-9_-"))
         (start     (point))
         (ignore    (skip-chars-forward "A-Za-z0-9_-"))
         (end       (point))
         (token     (buffer-substring-no-properties start end))
         )
    (goto-char initialpt)
    (cond
     ;; matches Jira issue IDs
     (
      (string-match-all jira-md/issue-id-regex token) (org-jira-get-issue token)
      )
     )
    ))


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

