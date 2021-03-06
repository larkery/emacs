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
             (featurep 'project)
             (not (file-remote-p (buffer-file-name)))
             (project-current nil))
    (let ((root (project-root)))
      (when (string-prefix-p root (buffer-file-name))
        (concat " ("
                (propertize (project-name)
                            'face 'underline
                            'mouse-face 'mode-line-highlight
                            'local-map (make-mode-line-mouse-map 'mouse-1 'project-dired))
                ")")))))

(defun mode-line-pad-right (rhs)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (let* ((the-rhs (format-mode-line rhs))
         (reserve (length the-rhs)))
    (list
     (propertize " " 'display `((space :align-to (- right ,reserve)))) rhs)))

(setq-default
 frame-title-format
 '(dired-directory dired-directory
                   
                   (buffer-file-name
                    (:eval
                     (if (and (not (file-remote-p buffer-file-name))
                              (project-current nil))
                         (list "[" (project-name) "]" " %b")
                      buffer-file-name))
                    "%b"))
 
 mode-line-format
 `("%5l "
   "%* "
   ,(propertize "%b"
               'mouse-face 'mode-line-highlight
               'local-map (make-mode-line-mouse-map 'mouse-1 'dired-from-buffer))
   (:eval (mode-line-get-project))
   (:eval (mode-line-get-host))
   (:eval (mode-line-pad-right
           `(,mode-line-misc-info " " ,mode-line-modes)))))

(setq-default icon-title-format frame-title-format)
