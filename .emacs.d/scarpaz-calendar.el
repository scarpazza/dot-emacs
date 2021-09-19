(calendar-set-date-style 'iso)

(setq holiday-bahai-holidays nil)
(setq holiday-general-holidays '(
                                 (holiday-fixed 1 1 "New Year's Day")
                                 (holiday-float 1 1 3 "Martin Luther King Day")
                                 (holiday-float 2 1 3 "President's Day")
                                 (holiday-float 5 1 -1 "Memorial Day")
                                 ;; (holiday-fixed 6 14 "Flag Day")
                                 (holiday-fixed 7 4 "Independence Day")
                                 (holiday-float 9 1 1 "Labor Day")
                                 (holiday-float 10 1 2 "Columbus Day")
                                 (holiday-fixed 11 11 "Veteran's Day")
                                 (holiday-float 11 4 4 "Thanksgiving"))
      )

(setq view-diary-entries-initially t
       mark-diary-entries-in-calendar t
       number-of-diary-entries 7
       general-holidays  nil
       )
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)


(eval-after-load "calendar"
  `(progn
     (define-key calendar-mode-map (kbd "<f5>")      'calendar-redraw)
     (define-key calendar-mode-map (kbd "<return>")  'diary-view-entries)
     (define-key calendar-mode-map (kbd "M-<right>") 'calendar-forward-month)
     (define-key calendar-mode-map (kbd "M-<left>")  'calendar-backward-month)))
