;; Set my favorite calendar mode defaults and key bindings.
;;
;; Most of the settings are self-explanatory.
;;
;; Features:
;;
;; - display and initial view customizations
;;
;; - extra column displaying the week number in the year (1 ... 52)
;;
;; - pressing any digit ranging from '2' to '9' will select
;;   progressively wider calendar layouts, with per-day columns
;;   ranging from 2 to 9 characters;
;;
;; - support for weeklies:
;;   (I typically organize my work on a weekly basis, via weekly ("weeklies") that are timestamped on Friday dates, and that I
;;   keep  under ~/.../weeklies/<year>/<date>, e.g.: ~/development/weeklies/2021/2021-09-24.md)
;;
;;   - Pressing 'w' or <return> on a calendar date will take you to the
;;     weekly document associated with the Friday of that week.
;;
;;
;; I made no changes to the diary functions.
;;
;; The source code contained in this file is released under the GNU
;; General Public License v3.  To learn more, see
;; https://www.gnu.org/licenses/gpl-3.0.en.html
;;
;;
;; Daniele Scarpazza


(require 'calendar)
(calendar-set-date-style 'iso)

;; I don't have any Bahai colleagues, so I don't need to see their holidays
(setq holiday-bahai-holidays nil)


(setq calendar-view-diary-initially-flag t
      calendar-mark-holidays-flag        t
      calendar-mark-diary-entries-flag   t
      calendar-latitude                  40.7
      calendar-longitude                 -74
      calendar-location-name             "New York City"
      calendar-time-zone                 -300
      calendar-standard-time-zone-name "EST"
      calendar-daylight-time-zone-name "EDT"
      calendar-left-margin               6
      calendar-day-header-array      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
      calendar-day-header-width          3
      calendar-day-digit-width           3
      calendar-abbrev-length             3
      calendar-column-width              4
      calendar-month-width              40
      )


(defun scarpaz/change-day-width (&optional arg)
  (interactive "p")
  (setq digit_pressed (- (aref (this-single-command-keys) 0) ?0))
  (when (< digit_pressed 2) (setq digit_pressed 2))
  (message " you pressed %i" digit_pressed)
  (setq calendar-day-header-width          digit_pressed
        calendar-day-digit-width           digit_pressed
        calendar-abbrev-length             digit_pressed
        calendar-intermonth-spacing        (+ 7 (* 2  digit_pressed))
        calendar-column-width              (+ 1 digit_pressed)
        calendar-month-width               (* 10 (+ 1 digit_pressed)))
  (calendar-redraw)
)

(copy-face 'calendar-weekend-header 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil :height 1.0)

(setq calendar-intermonth-text
      '(propertize
        (format " %2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-header-face))

(setq calendar-intermonth-header
      (propertize " Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'font-lock-function-name-face))


(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)


;; Unfinished - I haven't decided what I want to do here
(defun scarpaz/toggle-diary-view-entries (&optional arg)
  (interactive "p")
  (message "%s" (diary-view-entries))
  )


(defun scarpaz/display-date-info (&optional arg)
  (interactive "p")
  (diary-view-entries)
  (calendar-cursor-holidays)
  )

(defun scarpaz/insert-today (&optional arg)
  (interactive "p")
  (insert (calendar-date-string (calendar-current-date) ))
  )

(eval-after-load "calendar"
  `(progn

     ;; these keys stretch of compact the calendar horizontally
     (define-key calendar-mode-map (kbd "1")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "2")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "3")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "4")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "5")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "6")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "7")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "8")         'scarpaz/change-day-width)
     (define-key calendar-mode-map (kbd "9")         'scarpaz/change-day-width)

     ;; bind function keys to operations that are consistent with their
     ;; use elsewhere
     (define-key calendar-mode-map (kbd "<f2>")      'diary-insert-entry)
     (define-key calendar-mode-map (kbd "<f4>")      'diary-insert-entry)

     (define-key calendar-mode-map (kbd "+")         'diary-insert-entry)
     (define-key calendar-mode-map (kbd "=")         'diary-insert-entry)
     (define-key calendar-mode-map (kbd "SPC")       'scarpaz/display-date-info)

     (define-key calendar-mode-map (kbd "w")         'scarpaz/open-weekly)
     (define-key calendar-mode-map (kbd "<f5>")      'calendar-redraw)
     (define-key calendar-mode-map (kbd "<return>")  'scarpaz/open-weekly)
     (define-key calendar-mode-map (kbd "M-<right>") 'calendar-forward-month)
     (define-key calendar-mode-map (kbd "M-<left>")  'calendar-backward-month)))

(defun scarpaz/open-weekly (&optional arg)
  "Open my weekly notes for the week on which the cursor is located. Unfinished."
  (interactive "p")
  (calendar-end-of-week 1)
  (calendar-backward-day 1)

  (if (not (boundp 'scarpaz/weeklies_path))
      (message "Error: can't jump to the weekly report yet.
You must first define variable 'scarpaz/weeklies_path' to the directory where you hold your weekly reports.
A good place to do it is inside your '~/.emacs' file.
Do not include a trailing slash.")
    (if (not (file-directory-p scarpaz/weeklies_path))
        (message "Error: can't jump to the weekly report yet.
Your weekly report path %s (specified via variable 'scarpaz/weeklies_path') doesn't exist or is not a directory.
Create the directory or revise the variable contents." scarpaz/weeklies_path)
      (let* (
             ;; cursor-to-day returns 'American style' dates MM DD YYYY
             (date    (calendar-cursor-to-date t))
             (year    (nth 2 date))
             (month   (nth 0 date))
             (day     (nth 1 date))
             (filepath (format "%s/%04i/%04i-%02i-%02i.md" scarpaz/weeklies_path year year month day))
            )
        (message filepath)
        (switch-to-buffer (find-file-noselect filepath nil nil nil))
        )
      )
    )
  )
