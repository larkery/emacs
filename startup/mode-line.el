(defun mode-line-get-host ()
  (when (and (buffer-file-name)
             (file-remote-p (buffer-file-name)))
    (let ((parts (tramp-dissect-file-name (buffer-file-name))))
      (concat " "
              (tramp-file-name-user parts)
              "@"
              (tramp-file-name-host parts)
              ))))

(defun mode-line-get-project ()
  (when (and (buffer-file-name)
             (featurep 'projectile)
             (not (file-remote-p (buffer-file-name)))
             (projectile-project-p))
    (let ((root (projectile-project-root)))
      (when (string-prefix-p root (buffer-file-name))
        (concat " (" (propertize (projectile-project-name)
                                 'face 'italic
                                 ) ")")))))

(defun mode-line-pad-right (rhs)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (let* ((the-rhs (format-mode-line rhs))
         (reserve (length the-rhs)))
    (list (propertize " " 'display `((space :align-to (- right ,reserve)))) rhs)))

(setq-default
 frame-title-format
 '(dired-directory dired-directory (buffer-file-name buffer-file-name "%b"))

 mode-line-format
 '("%5l "
   "%b"
   (:eval (mode-line-get-project))
   (:eval (mode-line-get-host))
   (:eval (mode-line-pad-right
           `(,mode-line-misc-info " " ,mode-line-modes))))

 )

