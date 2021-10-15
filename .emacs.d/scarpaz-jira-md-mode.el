;;
;; jira-md mode
;;
;; Daniele P. Scarpazza - scarpaz@scarpaz.com
;;
;; This is a minor mode intended to make markdown mode editing more
;; interactive, in manners that are consistent with the markdown dialect
;; accepted by Atlassian (e.g., Bitbucket extensions).
;;
;; Specifically:
;;
;; - it colorizes
;;   - user mentions (e.g., @user)
;;   - jira issue numbers (e.g., PROJECT-1234)
;;   - dates in ISO or US format (e.g., 2010-12-23 or 1997/12/31)
;;
;; - it resolves user mentions via "finger" when you type
;;   or move your cursor on them;
;;   It uses a hash table to resolve usernames (and substrings) only once.
;;
;; - it displays a transient calendar when the cursor is over a date
;;
;; - it binds "Alt-Enter" to act on the element under the cursor:
;;   - for jira tickets,
;;     it looks the ticket up via org-jira;
;;   - for dates (ISO or US),
;;     it opens the calendar on that date.
;;
;; It distinguishes between ISO and US dates.  I don't recognize
;; European-style dates because they can't be distinguished from the
;; American-style ones just by syntax (the year is at the end in
;; both). This is no slight against Europe: I'm European by birth and
;; upbringing, and I prefer the ISO format. In fact, using
;; international standards is in the true nature of the European
;; character.
;;
;; It auto-enables when editing markdown files.
;;
;; The source code contained in this file is released under the GNU
;; General Public License v3.  To learn more, see
;; https://www.gnu.org/licenses/gpl-3.0.en.html
;;

(require 'font-lock)
(require 'org-jira)
(require 'calendar)
(provide 'jira-md-mode)

;; Customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup jira-md nil
  "Atlassian-oriented markdown integration, including user mentions and issue ."
  :prefix "jira-md/"
  :group 'jira-md
  :link "https://github.com/scarpazza/dot-emacs"
  )

(defcustom jira-md/email-regex
  "\\w+\\(\\.\\w+\\)?@\\(\\w\\|\\.\\)+"
  "Regular expression determining what tokens are recognized as email addresses. Play with it if you want."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

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



(setq jira-md/keywords
      (list
        (cons jira-md/email-regex     'font-lock-string-face)          ;; emails
        (cons jira-md/issue-id-regex  'font-lock-function-name-face) ;; jira issues
        (cons jira-md/user-id-regex   'font-lock-constant-face)      ;; user mentions
        (cons jira-md/iso-date-regex  'font-lock-keyword-face)       ;; dates
        (cons jira-md/us-date-regex   'font-lock-keyword-face)       ;; dates
        )
      )

;;;###autoload
(define-minor-mode jira-md-mode
  "Get your foos in the right places."
  :lighter " jira-md"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-<return>") 'scarpaz/go-jira)
            map)

  (font-lock-add-keywords nil jira-md/keywords)

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))
    )

  (setq scarpaz/user-hash-table (make-hash-table :test 'equal))
  (add-hook 'post-command-hook 'scarpaz/cursor-hook)
  )

;; TO DO: rewrite using looking-at

(defun scarpaz/go-jira ()
  (interactive)
  (let* (
         (initialpt (point))
         (ignore    (skip-chars-backward "A-Za-z0-9_/-"))
         (start     (point))
         (ignore    (skip-chars-forward "A-Za-z0-9_/-"))
         (end       (point))
         (token     (buffer-substring-no-properties start end))
         )
    (goto-char initialpt)
    (message "token %s" token)
    (cond
     ( (string-match-all jira-md/issue-id-regex token) (org-jira-get-issue token) )
     ( (string-match-all jira-md/iso-date-regex token) (scarpaz/go-to-date token) )
     ( (string-match-all jira-md/us-date-regex token)  (scarpaz/go-to-date token) )
     )
    )
  )

(defun scarpaz/parse-date-to-calendar-format (token)
  (setq parsed (mapcar 'string-to-number (split-string token "[-/]")))
  (if (string-match-all jira-md/iso-date-regex token)
      (let* ( (month (nth 1 parsed))
              (day   (nth 2 parsed))
              (year  (nth 0 parsed)) )
        (list month day year)
        )
    (if (string-match-all jira-md/us-date-regex token)
      (let* ( (month (nth 0 parsed))
              (day   (nth 1 parsed))
              (year  (nth 2 parsed)) )
        (list month day year)))
    )
  )


(defun scarpaz/go-to-date (token)
  (calendar)
  (calendar-goto-date (scarpaz/parse-date-to-calendar-format token)))

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


(defun string-match-all (pattern string)
  (and (eq 0 (string-match pattern string))
       (eq (length string) (match-end 0) )))

(setq scarpaz/popup_buffer nil)


;; 2021-1-13 test line


(defun scarpaz/peek-at-date ( date_string )
  "Peek at the current date.
This means popping up a quick calendar peek window centered on the date, if no calendar window already exists.
If a calendar window already exists and is visible, that just display the current date there.
The input is the date string, unparsed."

  (setq date (scarpaz/parse-date-to-calendar-format date_string))

  (if (get-buffer-window calendar-buffer)
      ;; A *Calendar* window already exists - use it to peek
      (progn
        (with-selected-window (get-buffer-window calendar-buffer)
          (calendar-goto-date date)
          (calendar-mark-visible-date date 'calendar-peek-date)
          (calendar-cursor-to-visible-date date)
        )
        )
    ;;  No *Calendar* window exists - create my owns
    (with-current-buffer scarpaz/popup_buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (calendar-generate (car date) (nth 2 date) )
      (calendar-mode)
      (display-buffer-in-side-window (current-buffer)
                                   '(display-buffer-reuse-window . ((inhibit-same-window . t))))
      (with-selected-window (get-buffer-window (current-buffer))
        (calendar-cursor-to-visible-date date)
        (scarpaz/calendar-mark-visible-date date 'calendar-peek-date)
        )))
  )


(defun scarpaz/cursor-hook ()
  (interactive)
  (if (not scarpaz/popup_buffer)
      (setq scarpaz/popup_buffer  (generate-new-buffer "*calendar peek*")))
  (setq recognized nil)
  (let* ( ( bounds (bounds-of-thing-at-point 'symbol) )
          ( beg    (car bounds))
          ( end    (cdr bounds)) )
    (when beg
        (when (char-before beg)
          (if (char-equal ?@ (char-before beg))
              ;; it's a user mention
              (progn
                (setq str (buffer-substring-no-properties beg end))
                (scarpaz/display-user-mention str beg end)
                (setq recognized 't)
                )
            ;; it's something else
            (progn
              (setq str (buffer-substring-no-properties beg end))
              (setq date nil)
              (when (or (string-match-all jira-md/iso-date-regex str)
                        (string-match-all jira-md/us-date-regex str) )
                (setq recognized 't)
                (scarpaz/peek-at-date str)
                ))
              )))
    (when (not recognized) (delete-windows-on scarpaz/popup_buffer))
))


(add-hook 'post-command-hook 'scarpaz/cursor-hook)


(defface calendar-peek-date
  '((t :foreground "white"
       :background "RoyalBlue2"
       :weight bold
    ;; :underline t
       ))
  "Face for indicating the date under the cursor."
  :group 'calendar-faces)


;;;###autoload
(add-hook 'markdown-mode-hook 'jira-md-mode)


(defun scarpaz/calendar-mark-visible-date (date &optional mark)
  ;; Lazy copy of calendar-mark-visible-date, but operating on my pop-up buffer
  (if (calendar-date-is-valid-p date)
      (with-current-buffer scarpaz/popup_buffer
        (save-excursion
          (calendar-cursor-to-visible-date date)
          (setq mark
                (or (and (stringp mark) (= (length mark) 1) mark) ; single-char
                          (and (listp mark) (> (length mark) 0) mark) ; attrs
                          (and (facep mark) mark) ; )) face-name
                          diary-entry-marker))
          (cond
           ;; Face or an attr-list that contained a face.
           ((facep mark)
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face mark))
           ;; Single-character mark, goes after the date.
           ((and (stringp mark) (= (length mark) 1))
            (overlay-put
             (make-overlay (1+ (point)) (+ 2 (point))) 'display mark))
           (t                           ; attr list
            (overlay-put
             (make-overlay (1- (point)) (1+ (point))) 'face
             (calendar-make-temp-face mark))))))))
