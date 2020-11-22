(defun archive-extract-to-file (archive-name item-name command dir)
  "Extract ITEM-NAME from ARCHIVE-NAME using COMMAND. Save to
DIR."
  (unwind-protect
      ;; remove the leading / from the file name to force
      ;; expand-file-name to interpret its path as relative to dir
      (let* ((file-name (if (string-match "\\`/" item-name)
                            (substring item-name 1)
                          item-name))
             (output-file (expand-file-name file-name dir))
             (output-dir (file-name-directory output-file)))
        ;; create the output directory (and its parents) if it does
        ;; not exist yet
        (unless (file-directory-p output-dir)
          (make-directory output-dir t))
        ;; execute COMMAND, redirecting output to output-file
        (apply #'call-process
               (car command)            ;program
               nil                      ;infile
               `(:file ,output-file)    ;destination
               nil                      ;display
               (append (cdr command) (list archive-name item-name))))
    ;; FIXME: add unwind forms
    nil))

(defun archive-extract-marked-to-file (output-dir)
  "Extract marked archive items to OUTPUT-DIR."
  (interactive "GOutput directory: ")
  (let ((command (symbol-value (archive-name "extract")))
        (archive (buffer-file-name))
        (items (archive-get-marked ?* t))) ; get marked items; t means
                                           ; get item under point if
                                           ; nothing is marked
    (mapc
     (lambda (item)
       (archive-extract-to-file archive
                                (aref item 0) ; get the name from the descriptor
                                command output-dir))
     items)))

(provide 'archive-extract-to-file)
