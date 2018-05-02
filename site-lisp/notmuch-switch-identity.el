(defun notmuch-update-fcc ()
  (save-mark-and-excursion
      (message-goto-fcc)
      (let ((bounds (bounds-of-thing-at-point 'line)))
        (delete-region (car bounds) (cdr bounds)))
      (notmuch-fcc-header-setup)
      ))

(defun notmuch-select-draft-folder (o &rest args)
  (let ((selected-folder notmuch-draft-folder))
    (save-mark-and-excursion
          (message-goto-from)
          (let ((from-text
                 (buffer-substring
                  (save-excursion (message-beginning-of-line) (point))
                  (point)))
                (possible-folders notmuch-draft-folders))
            
            (while possible-folders
              (let* ((folder (car possible-folders))
                     (pattern (car folder))
                     )
                (if (string-match-p pattern from-text)
                    (progn (setq-local selected-folder (cdr folder))
                           (setq possible-folders nil))
                  (setq possible-folders (cdr possible-folders)))))))

    (let ((notmuch-draft-folder selected-folder))
      (apply o args))))

(defun notmuch-switch-identity ()
  (interactive)
  (save-excursion
    (message-goto-from)
    (let* ((from-end (point))
           (from-start (progn
                         (message-beginning-of-line)
                         (point)))
           (from-text (buffer-substring from-start from-end))
           (mails notmuch-identities))
      (while (and mails (not (string= from-text (car mails))))
        (setq mails (cdr mails)))

      (let ((new-from (or (cadr mails)
                          (car notmuch-identities))))
        (delete-region from-start from-end)
        (insert new-from)
        (notmuch-update-fcc)))))

(add-hook 'notmuch-mua-send-hook 'notmuch-update-fcc)
(advice-add 'notmuch-draft-save :around 'notmuch-select-draft-folder)

(provide 'notmuch-switch-identity)
