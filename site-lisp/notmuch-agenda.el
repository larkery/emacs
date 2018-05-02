;; integrate notmuch messages with org-mode agenda, by:
;; - showing agenda in message body where there's an invitation
;; - adding buttons to respond to messages

(defun notmuch-agenda-datetime-as-iso (datetime)
  "Convert a date retrieved via `icalendar--get-event-property' to ISO format."
  (if datetime
    (format "%04d-%02d-%02d"
      (nth 5 datetime)                  ; Year
      (nth 4 datetime)                  ; Month
      (nth 3 datetime))))

(defun notmuch-agenda-event-time (event zone-map property)
  "Given an EVENT and a ZONE-MAP, turn the icalendar timestamp
  for PROPERTY into an emacs internal time representation"
  (let* ((timestamp (icalendar--get-event-property event property))
         (zone (icalendar--find-time-zone (icalendar--get-event-property-attributes event property)
                                          zone-map)))
    (icalendar--decode-isodatetime timestamp nil zone)))

(defun notmuch-agenda-relative-date (reference timestamp)
  (let* ((encoded-reference (apply 'encode-time reference))
         (encoded-timestamp (apply 'encode-time timestamp))

         (days (/ (float-time (time-subtract encoded-timestamp encoded-reference))
                  86400))
         (past (< days 0))
         (abs-days (abs days))

         (day-part
          (cond
           ((< abs-days 1) "today") ;; not quite right I guess?
           ((< abs-days 2) (if past "yesterday" "tomorrow"))
           ((< abs-days 8) (concat (if past "last" "next")
                                   (format-time-string " %A" encoded-timestamp)
                                   ))
           ((and (not past) (< abs-days 15))
            (concat "a week next"
                    (format-time-string " %A" encoded-tiemstamp)
                    ))
           (t (format "%s, %d week%s %s" (format-time-string "%A %e %B %Y" encoded-timestamp)
                      (floor (/ abs-days 7))
                      (if (= 1 (floor (/ abs-days 7))) "" "s")
                      (if past "ago" "time")
                      ))
           )))
    
    day-part))

(defun notmuch-agenda-friendly-date (dtstart dtend rrule rdate duration)
  (let* ((now (decode-time (current-time)))
         (start-time (format-time-string "%H:%M" (apply 'encode-time dtstart)))
         (rel-date (notmuch-agenda-relative-date now dtstart)))
    ;; TODO handle other date structures
    (concat start-time " " rel-date)))

(defun notmuch-agenda-org-date (dtstart-dec dtend-dec rrule rdate duration)
  (let* ((start-d (notmuch-agenda-datetime-as-iso dtstart-dec))
         (start-t (icalendar--datetime-to-colontime dtstart-dec))
         
         end-d end-t)

    (setq end-d (if dtend-dec
                    (notmuch-agenda-datetime-as-iso dtend-dec)
                  start-d))
    
    (setq end-t (if dtend-dec
                    (icalendar--datetime-to-colontime dtend-dec)
                  start-t))
    
      (if (equal start-d end-d)
          (format "<%s %s-%s>" start-d start-t end-t)
        (format "<%s %s>--<%s %s>" start-d start-t end-d end-t))))

(defun notmuch-agenda-insert-part (msg part content-type nth depth button)
  (let (icalendar-element)
    (with-temp-buffer
      ;; Get the icalendar text and stick it in a temp buffer
      (insert (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
      ;; Transform CRLF into LF
      (goto-char (point-min))
      (while (re-search-forward "\r\n" nil t) (replace-match "\n" nil nil))
      ;; Unfold the icalendar text so it can be parsed
      (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
      ;; Go to the first VCALENDAR object in the result
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VCALENDAR\\s-*$")
        (beginning-of-line)
        (setq icalendar-element (icalendar--read-element nil nil)))
      ;; Dispose of the junk buffer produced by icalendar--get-unfolded-buffer
      (kill-buffer (current-buffer)))

    (when icalendar-element
      (let* ((events (icalendar--all-events icalendar-element))
             (zone-map (icalendar--convert-all-timezones events)))
        (dolist (event events)
          ;; insert event description string
          (let* ((summary (icalendar--get-event-property event 'SUMMARY))
                 (comment (icalendar--get-event-property event 'COMMENT))
                 (location (icalendar--get-event-property event 'LOCATION))
                 (organizer (icalendar--get-event-property event 'ORGANIZER))
                 (attendees (icalendar--get-event-properties event 'ATTENDEE))
                 (summary (when summary (icalendar--convert-string-for-import summary)))
                 (comment (when comment (icalendar--convert-string-for-import comment)))

                 (dtstart (notmuch-agenda-event-time event zone-map 'DTSTART))
                 (dtend (notmuch-agenda-event-time event zone-map 'DTEND))
                 (rrule (icalendar--get-event-property event 'RRULE))
                 (rdate (icalendar--get-event-property event 'RDATE))
                 (duration (icalendar--get-event-property event 'DURATION))
                 
                 
                 (friendly-date (notmuch-agenda-friendly-date dtstart dtend rrule rdate duration))
                 )

            (when friendly-date (insert friendly-date "\n"))
            (when summary (insert "Summary: "summary"\n"))
            (when comment (insert "Comment: "comment"\n"))
            (when location (insert "Location: "location"\n"))
            (when organizer (insert "Organizer: "organizer"\n"))
            (when attendees (insert "Attending: \n"))
            (while attendees
              (insert "  - " (car attendees) "\n")
              (setq attendees (cdr attendees)))))
        t))))


(fset 'notmuch-show-insert-part-text/calendar #'notmuch-agenda-insert-part)

(provide 'notmuch-agenda)
