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
      (goto-char end))))

(provide 'org-extras)
