;;
;; Test harness for minor mode jira-md.
;;
;;
;; You don't need to use this file unless you customize the jira-md/*-regex variables and you want
;; to make sure they still recognize known-good values.
;;
;;
;; All files in this repository are released under the GNU General Public License v3.  To learn
;; more, see https://www.gnu.org/licenses/gpl-3.0.en.html


(add-to-list 'load-path "~/.emacs.d")

(require 'jira-md-mode "scarpaz-jira-md-mode.el")
(require 'cl-lib)


(defun string-match-all (pattern string)
  (and (eq 0 (string-match pattern string))
       (eq (length string) (match-end 0) )))

(defun jira-md/tests ()
  (cl-assert (string-match-all jira-md/issue-id-regex "ABC-1"))
  (cl-assert (string-match-all jira-md/issue-id-regex "PROJECT9-123"))
  (cl-assert (not (string-match-all jira-md/issue-id-regex "ABC-A")))
  (cl-assert (not (string-match-all jira-md/issue-id-regex "123-A")))

  ;; test ISO dates
  (setq valid-iso-dates '( "1997-12-23" "1997-07-31" "1997/12/23" "1997/9/23" "1997/9/9"
                           "1997/10/23" "1997/11/23" "1997/12/23" ))
  (dolist (x valid-iso-dates) (cl-assert  (string-match-all jira-md/iso-date-regex x)) )

  (setq invalid-iso-dates '( "1997/13/01" "1997/0/12" "1997/7/70" "1997-07-41" "1997-23-12"   ))
  (dolist (x invalid-iso-dates) (cl-assert  (not(string-match-all jira-md/iso-date-regex x))) )


  ;; test US dates
  (setq valid-us-dates '("07-31-1998" "12/23/1998" "10/01/1998" "11/01/1998" "12/01/1998"
                         "7/23/1998" "7/7/1998"))
  (dolist (x valid-us-dates) (cl-assert   (string-match-all jira-md/us-date-regex x)))

  (setq invalid-us-dates '( "13/01/1998" "0/12/1998" "7/70/1998" "07-41-1998" "23-12-1998" ))
  (dolist (x invalid-us-dates) (cl-assert  (not (string-match-all jira-md/us-date-regex x))) )
  
  )
