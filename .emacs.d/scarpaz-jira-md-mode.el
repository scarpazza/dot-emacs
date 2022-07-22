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
;; - it displays a transient calendar peek when the cursor is over a date
;;
;; - it binds "Alt-Enter" to act on the element under the cursor:
;;   - for jira tickets,
;;     it looks the ticket up via org-jira;
;;   - for dates (ISO or US),
;;     it opens the calendar on that date.
;;   - for expressions ending in an equal sign (=),
;;     evaluate them via python
;;
;; It distinguishes between ISO and US dates.  I chose not to
;; recognize European-style dates because they can't be distinguished
;; from American-style ones just by syntax (the year is at the end in
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

;; TO DO - helm ffap helm-ffap-guesser

(require 'font-lock)
(require 'org-jira)
(require 'calendar)
(require 'yafolding)
(require 'thingatpt)



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

(defface calendar-peek-date
  '((t :foreground "white"
       :background "RoyalBlue2"
       :weight bold
    ;; :underline t
       ))
  "Face for indicating the date under the cursor."
  :group 'calendar-faces)


(defcustom jira-md/absolute-path-regex
  "\\(/[-~/[:alnum:]_.${}#%,:]+\\)+"
  "Regular expression describing absolute pathnames. Change this if you need to recognize stranger pathnames."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

(defcustom jira-md/relative-path-regex
  "\\.\\(\\.\\)?\\(/[-~/[:alnum:]_.${}#%,:]+\\)+"
  "Regular expression describing relative pathnames. Change this if you need to recognize stranger pathnames."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)

(defcustom jira-md/python-interpreter
  "/usr/bin/python"
  "Path to the python interpreter, used to evaluate math expressions."
  :type 'string  :require 'jira-md-mode  :group 'jira-md)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scarpaz/popup_buffer nil)

;; TO DO enforce whole word
(setq jira-md/day-name-regex
      (concat ;"\\([^a-zA-Z]\\)"
       "\\("
      (mapconcat 'identity calendar-day-name-array "\\|")
      ;;        "\\)\\($\\|[^a-zA-Z]
      "\\)"
      ) )

(setq jira-md/day-abbrev-regex
      (concat "\\("
              (mapconcat 'identity calendar-day-abbrev-array "\\|")
              "\\)"))

(message "jira-md: week day names are %s, abbreviated as %s - to localize, customize variables 'calendar-day-name-[abbrev-]array'."
         jira-md/day-name-regex jira-md/day-abbrev-regex)

(setq jira-md/keywords
      (list
       (cons jira-md/relative-path-regex 'font-lock-builtin-face)       ;; rel path names
       (cons jira-md/absolute-path-regex 'font-lock-builtin-face)       ;; abs path names
       (cons jira-md/email-regex         'font-lock-string-face)        ;; email addresses
       (cons jira-md/issue-id-regex      'font-lock-function-name-face) ;; jira issues
       (cons jira-md/user-id-regex       'font-lock-constant-face)      ;; user mentions
       (cons jira-md/iso-date-regex      'font-lock-keyword-face)       ;; dates.
       (cons jira-md/us-date-regex       'font-lock-keyword-face)       ;; dates
       (cons jira-md/day-name-regex      'font-lock-keyword-face )      ;; names of days of the week
       (cons jira-md/day-abbrev-regex    'font-lock-keyword-face )      ;; abbreviated names of days of the week
       )
      )



;;;###autoload
(define-minor-mode jira-md-mode
  "Get your foos in the right places."
  :lighter " jira-md"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-<return>") 'scarpaz/act-on-element)
            ;;(define-key map (kbd "C-<right>")  'markdown-demote)
            ;;(define-key map (kbd "C-<left>")   'markdown-promote)
            (define-key map (kbd "M-=")        'outline-show-subtree)
            (define-key map (kbd "M--")        'outline-hide-subtree)
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


(defun pt-token ()
  "Gets the text matched by the last thing-at-point-looking-at invocation."
  (buffer-substring-no-properties (match-beginning 0) (match-end 0)))

(defun jira-md/open-file (filename)
  (message "jira-md act on path name: opening %s" filename)
  (find-file filename)
  )

(defun jira-md/eval-via-python (expr)
  (string-match "[[:blank:]]+=[[:blank:]]*" expr)
  (setq cleanexpr (substring expr 0 (match-beginning 0) ))
  (setq result (shell-command-to-string (concat jira-md/python-interpreter " -c 'from math import *; print(" cleanexpr ")'")))
  ; skip the new line character at the end
  (setq result (substring result 0 (- (length result) 1)))
  (message "result from python: %s" result)
  (insert result)
  )

(defun scarpaz/act-on-element ()
  "Act on the element under the cursor. If it's a JIRA issue number, open it.
   If it's a date, open the calendar on it"
  (interactive)
  (cond
   ( (thing-at-point-looking-at ".*[[:blank:]]+=[[:blank:]]*") (jira-md/eval-via-python (pt-token) ))
   ( (thing-at-point-looking-at jira-md/relative-path-regex) (jira-md/open-file   (file-truename (pt-token))))
   ( (thing-at-point-looking-at jira-md/absolute-path-regex) (jira-md/open-file   (pt-token)))
   ( (thing-at-point-looking-at jira-md/iso-date-regex     ) (scarpaz/go-to-date  (pt-token)))
   ( (thing-at-point-looking-at jira-md/us-date-regex      ) (scarpaz/go-to-date  (pt-token)))
   ( (thing-at-point-looking-at jira-md/day-name-regex     ) (scarpaz/expand-date (pt-token)))
   ( (thing-at-point-looking-at jira-md/day-abbrev-regex   ) (scarpaz/expand-date (pt-token)))
   ( (thing-at-point-looking-at "^[[:blank:]]*[-+*]"       ) (yafolding-toggle-element))
   ( (thing-at-point-looking-at jira-md/issue-id-regex     ) (org-jira-get-issue  (token) ))
   )
  )



(defun scarpaz/expand-date (token start end)
  (setq DoW_idx (seq-position calendar-day-name-array token))
  (if (not DoW_idx)
    (setq DoW_idx (seq-position calendar-day-abbrev-array token)))
  (when DoW_idx
    (setq today (calendar-current-date))
    (setq dow
          (calendar-gregorian-from-absolute
           (+ (- (calendar-absolute-from-gregorian today)
                 (calendar-day-of-week today))
              DoW_idx)))
    (goto-char start)
    (delete-region start end)
    (insert (calendar-date-string dow))
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
          (right-char)
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
        (scarpaz/calendar-mark-visible-date date 'calendar-peek-date)
        (calendar-cursor-to-visible-date date)
        (right-char)

        )))
  )


(defun scarpaz/cursor-hook ()
  "This hook gives you a peek at user mentions, dates and in the future other active elements."
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
    (when (and (not recognized)
               (buffer-live-p scarpaz/popup_buffer))
               (delete-windows-on scarpaz/popup_buffer))
))


(add-hook 'post-command-hook 'scarpaz/cursor-hook)

;;;###autoload
(add-hook 'markdown-mode-hook 'jira-md-mode)
(add-hook 'markdown-mode-hook 'yafolding-mode) ;; I write deep and long itemized lists and need convenient ways to collapse them.

(add-hook 'markdown-mode-hook '(lambda ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)))

(defun scarpaz/calendar-mark-visible-date (date &optional mark)
  ;; Lazy copy of calendar-mark-visible-date, but operating on my pop-up buffer.
  ;; This is ugly.
  ;; Unclear how I'll fix this.
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


;;;
(provide 'jira-md-mode)
