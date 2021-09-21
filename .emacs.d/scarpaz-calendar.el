;; Set my favorite calendar mode defaults and key bindings.
;;
;; Most of the settings are self-explanatory.
;;
;; An important change is that I typically work on weekly documents
;; ("weeklies") that are timestamped on Friday dates, and that I
;; organize under ~/.../weeklies/<year>/<date>, e.g.:
;; ~/development/weeklies/2021/2021-09-24.md
;;
;; Pressing 'w' or <return> on a calendar date will take you to the weekly
;; document associated with the Friday of that week.
;;
;; I made no changes to the old diary functions.
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
      calendar-intermonth-spacing        20
      calendar-left-margin               6
      calendar-day-header-array      ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"]
      calendar-day-header-width           3
      calendar-day-digit-width           3
      calendar-abbrev-length             3
      calendar-column-width              4
      calendar-month-width              40
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


(eval-after-load "calendar"
  `(progn
     (define-key calendar-mode-map (kbd "+")         'diary-insert-entry)
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

