(defun org-goto-path (path &optional ff-command)
    "Find / create an org heading. PATH is a list of strings.
First string is a filename, subsequent strings are heading names, each being
a subheading of the last. Return the buffer, which will be positioned at the heading."
    (let ((ff-command (or ff-command #'find-file))
          (start (car path))
          (path (cdr path)))
      (with-current-buffer
          (funcall ff-command start)
        (save-restriction
          (widen)
          (goto-char (point-min))
          (cl-loop for part in path
                   for ix from 1
                   do
                   (setq part (format "%s %s" (make-string ix ?*) part))
                   (unless (search-forward-regexp (rx-to-string `(seq bol ,part)) nil t)
                     (goto-char (point-max))
                     (insert "\n" part)
                     (unless (looking-at "\n")
                       (save-excursion (insert "\n"))))
                   (beginning-of-line)
                   (org-narrow-to-element)))
        (current-buffer))))

(defcustom org-log-location
  '("~/notes/journal/%Y/%B.org"
    "[%Y-%m-%d %a]")
  "A list of strings, each of which will be used with `format-time-string'.
The first will find a file, and the rest headings and subheadings and so on.")

(defun org-log-goto (&optional date)
  (interactive)

  (when (eq major-mode 'calendar-mode)
    (let ((dt (calendar-cursor-to-date)))
      (when dt
        (setq date (encode-time 0 0 0 (nth 1 dt) (nth 0 dt) (nth 2 dt))))))

  (with-current-buffer
      (org-goto-path (mapcar (lambda (x) (format-time-string x date))
                             org-log-location))
    (outline-show-subtree)
    (org-narrow-to-subtree)
    (let ((here (point))
          (end (point-max)))
      (widen)
      (outline-hide-sublevels 1)
      (goto-char here)
      (outline-show-subtree)
      (goto-char end)
      (unless (looking-at "^$")
        (insert "\n"))
      (insert "- ")
      (insert (format-time-string "%H:%M "))
      )))

(defvar org-notify-check-timer nil)
(defvar org-notify-check-interval 600)

(defun org-notify ()
  "Turn on/off notification for org agenda events"
  (interactive)

  (if org-notify-check-timer
      (progn
        (cancel-timer org-notify-check-timer)
        (setq org-notify-check-timer nil)
        (mapc #'cancel-timer org-notify-extant-timers)
        (remove-hook 'after-revert-hook 'org-notify-check-if-org)
        (remove-hook 'after-save-hook 'org-notify-check-if-org)
        (message "Agenda notifications disabled"))
    (progn
      (require 'notifications)
      (add-hook 'after-save-hook 'org-notify-check-if-org)
      (add-hook 'after-revert-hook 'org-notify-check-if-org)
      (setq org-notify-check-timer
            (run-with-timer 0 org-notify-check-interval #'org-notify-check))
      
      (message "Agenda notifications enabled"))))


(defvar org-notify-extant-timers nil)
(defvar org-notify-soon-headings nil)

(defun org-notify-check-if-org ()
  (when (eq major-mode 'org-mode)
    (org-notify-check)))

(defun org-notify-check ()
  "Check for any events that are coming soon and schedule a beep for them"
  (message "Checking for org notifications")
  (mapc #'cancel-timer org-notify-extant-timers)
  (setq org-notify-soon-headings nil)
  (org-map-entries
   'org-notify-check-headline
   "TIMESTAMP>=\"<now>\""
   'agenda)
  (org-map-entries
   'org-notify-check-headline
   "SCHEDULED>=\"<now>\""
   'agenda))

(defun org-notify-check-headline ()
  (let* ((heading (org-get-heading t t t t))
         (time-string (or (org-entry-get (point) "SCHEDULED")
                          (org-entry-get (point) "TIMESTAMP")))
         (time-secs (org-time-string-to-seconds time-string))
         (delta-t (- time-secs (time-to-seconds)))
         (early-warning (- time-secs (* 60 5))))
    (when (< delta-t org-notify-check-interval)
      (message "Will notify for %s" heading)
      (push message org-notify-soon-headings)
      (push
       (run-at-time (seconds-to-time early-warning)
                    nil
                    'notifications-notify
                    :title
                    (format "%s (5 minutes)" heading))
       org-notify-extant-timers)
      (push (run-at-time (seconds-to-time time-secs)
                         nil
                         'notifications-notify
                         :urgency 'critical
                         :title
                         (format "%s" heading))
            org-notify-extant-timers))))

(provide 'org-extras)
