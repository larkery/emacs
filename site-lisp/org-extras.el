;; -*- lexical-binding: t -*-

(defun org-goto-path (path &optional ff-command template-text)
    "Find / create an org heading. PATH is a list of strings.
First string is a filename, subsequent strings are heading names, each being
a subheading of the last. Return the buffer, which will be positioned at the heading."
    (let ((ff-command (or ff-command #'find-file))
          (start (car path))
          (path (cdr path)))
      (with-current-buffer
          (funcall ff-command start)
        (when (zerop (buffer-size))
          (insert template-text)
          (goto-char (point-max)))
        (when path
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
                     (org-narrow-to-element))))
        (current-buffer))))

(defcustom org-log-location
  '("~/notes/journal/%Y/%F W%V.org")
  "A list of strings, each of which will be used with `format-time-string'.
The first will find a file, and the rest headings and subheadings and so on.")

(defcustom org-log-template
  (lambda (date)
    (format-time-string "#+TITLE: Journal for %Y %W " date))

  "Function creating text to insert into a new journal file, called with date")

(defun find-previous-file-in-directory ()
  (interactive)
  (let* ((dir (file-name-directory buffer-file-name))
         (file (file-name-nondirectory buffer-file-name))
         (files (directory-files dir)))
    (while (and files (not (string= file (second files))))
      (setq files (cdr files)))
    (if files
        (find-file (car files))
      (error "No previous file"))))

(defun find-next-file-in-directory ()
  (interactive)
  (let* ((dir (file-name-directory buffer-file-name))
         (file (file-name-nondirectory buffer-file-name))
         (files (directory-files dir)))
    (while (and files (not (string= file (car files))))
      (setq files (cdr files)))
    (if files
        (find-file (second files))
      (error "No next file"))))

(define-minor-mode org-log-mode "Extra keys for org log" nil "-log"
  (let ((k (make-sparse-keymap)))
    (define-key k (kbd "C-c C-p") 'find-previous-file-in-directory)
    (define-key k (kbd "C-c C-n") 'find-next-file-in-directory)
    (define-key k (kbd "C-c C-d") 'calendar)
    k))

(defun org-log-goto (&optional date)
  (interactive)

  (when (eq major-mode 'calendar-mode)
    (let ((dt (calendar-cursor-to-date)))
      (when dt
        (setq date (encode-time 0 0 0 (nth 1 dt) (nth 0 dt) (nth 2 dt))))))
  
  (with-current-buffer
      (org-goto-path (mapcar (lambda (x) (format-time-string x date))
                             org-log-location)
                     #'find-file
                     (funcall org-log-template date))
    (if (cdr org-log-location)
        (progn
          (outline-show-subtree)
          (org-narrow-to-subtree)
          (let ((here (point))
                (end (point-max)))
            (widen)
            (outline-hide-sublevels 1)
            (goto-char here)
            (outline-show-subtree)
            (goto-char end)))
      (goto-char (point-max)))
    (unless (looking-at "^$")
      (insert "\n"))
    (unless date
      (insert "- ")
      (insert (format-time-string "%H:%M ")))
    (org-log-mode)))


(defvar org-notify-check-timer nil)
(defvar org-notify-check-interval 600)

(defun org-notify (&optional state)
  "Turn on/off notification for org agenda events"
  (interactive)

  (let ((state (or state (if org-notify-check-timer -1 1))))
    (cond
     ((and (= -1 state) org-notify-check-timer)
      (progn
        (cancel-timer org-notify-check-timer)
        (setq org-notify-check-timer nil)
        (mapc #'cancel-timer org-notify-extant-timers)
        (remove-hook 'after-revert-hook 'org-notify-check-if-org)
        (remove-hook 'after-save-hook 'org-notify-check-if-org)
        (message "Agenda notifications disabled")))
     ((and (= 1 state) (not org-notify-check-timer))
      (progn
        (require 'notifications)
        (add-hook 'after-save-hook 'org-notify-check-if-org)
        (add-hook 'after-revert-hook 'org-notify-check-if-org)
        (setq org-notify-check-timer
              (run-with-timer 0 org-notify-check-interval 'org-notify-check))
        
        (message "Agenda notifications enabled"))))))

(defvar org-notify-extant-timers nil)
(defvar org-notify-soon-headings nil)

(defun org-notify-check-if-org ()
  (when (and (eq major-mode 'org-mode)
             (member buffer-file-name (org-agenda-files t)))
    (org-notify-check)))

(defun org-notify-display-notification (title location delay)
  (let ((is-url (and location
                     (string-match
                      (rx bos (| "zoommtg" (seq "http" (? "s"))) "://")
                      location))))
    (notifications-notify
     :title (format "%s (%s)"
                    title
                    (if (zerop delay) "now" (format "%dm" delay)))
     :actions
     (when is-url (list "default" "Go to location"))
     :on-action
     (when is-url (lambda (&rest args)
                    (message "%s" args)
                    (browse-url location)))

     :urgency (when (zerop delay) 'critical))))

(defun org-notify-check ()
  "Check for any events that are coming soon and schedule a beep for them"
  (require 'calendar)
  (require 'org-agenda)
  (mapc #'cancel-timer org-notify-extant-timers)
  
  (save-excursion
    (let ((date (calendar-current-date))
          (org-agenda-buffer nil)
          (next-appt-time nil)
          (next-appt-text ""))
      
      (dolist (f (org-agenda-files))
        (condition-case nil
            (progn (find-file-noselect f)
                   (dolist (e (org-agenda-get-day-entries
                               f date :scheduled :timestamp))
                     (let* ((hd-marker (get-text-property 0 'org-hd-marker e))
                            (marker (get-text-property 0 'org-marker e))
                            (heading (save-excursion
                                       (set-buffer (marker-buffer hd-marker))
                                       (goto-char (marker-position hd-marker))
                                       (org-get-heading t t t t)
                                       ))
                            (location (org-entry-get hd-marker "LOCATION"))
                            (is-url (and location
                                         (string-match
                                          (rx bos "http" (? "s") "://")
                                          location)))
                            (timestamp (save-excursion
                                         (set-buffer (marker-buffer marker))
                                         (goto-char (marker-position marker))
                                         (and (looking-at org-ts-regexp3)
                                              (match-string 0))))
                            )
                       (when timestamp
                         (let* ((time (decode-time (org-time-string-to-time timestamp)))
                                (sec (elt time 0))
                                (mins (elt time 1))
                                (hrs (elt time 2))
                                (time (decode-time)))
                           
                           (setf (elt time 0) sec
                                 (elt time 1) mins
                                 (elt time 2) hrs)
                           
                           (let ((location location))
                             (let* ((time (time-to-seconds (apply 'encode-time time)))
                                    (delta (- time (time-to-seconds))))
                               (when (< 0 delta (* 2 org-notify-check-interval))
                                 (when (or (not next-appt-time)
                                           (< time next-appt-time))
                                   (setq next-appt-time time
                                         next-appt-text (format "%02d:%02d %s"  hrs mins heading)))
                                 
                                 (push
                                  (run-at-time (seconds-to-time (- time (* 60 5)))
                                               nil
                                               'org-notify-display-notification
                                               heading location 5)
                                  org-notify-extant-timers)

                                 (push (run-at-time (seconds-to-time time)
                                                    nil
                                                    'org-notify-display-notification
                                                    heading location 0)
                                       
                                       org-notify-extant-timers)))))))

                     ))
          (error nil)))
      )))

(provide 'org-extras)
