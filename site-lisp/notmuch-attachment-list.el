


(define-derived-mode notmuch-attachment-list-mode
  tabulated-list-mode
  "Attachment List"
  "Major mode for listing attachments"
  (setq tabulated-list-format
        `[("Subject" 20 t)
          ("Date" 10 t)
          ("From" 10 t)
          ("Name" 0 t)]
        tabulated-list-padding 2
        tabulated-list-entries #'notmuch-attachment-list)
  (tabulated-list-init-header))

(defvar notmuch-attachment-list-query nil)
(make-variable-buffer-local 'notmuch-attachment-list-query)

(defun notmuch-attachments-from-part (part)
  (let ((disp (plist-get part :content-disposition))
        (con (plist-get part :content)))
    (cond
     ((and con (listp con))
      (mapcan #'notmuch-attachments-from-part con))
     ((string= "attachment" disp)
      (list part)))))

(defun notmuch-attachments-from-tree (tree)
  (let ((msg (car tree))
        (msgs (mapcan #'notmuch-attachments-from-tree (cadr tree))))
    (if msg
        (cons
         (cons msg (mapcan #'notmuch-attachments-from-part (plist-get msg :body)))
         msgs)
      msgs)))

(defun notmuch-attachments-from-thread (thread)
  (mapcan #'notmuch-attachments-from-tree thread))

(defun notmuch-attachment-list ()
  (let* ((forest (notmuch-call-notmuch-sexp
                  "show"
                  "--entire-thread=false"
                  "--format=sexp"
                  (format "tag:attachment and (%s)" notmuch-attachment-list-query)))
         
         (atts (mapcan #'notmuch-attachments-from-thread forest)))

    ;; each thing in atts is a message
    ;; and each message has a list of attachments

    (cl-loop for (msg . parts) in atts
             nconc
             (cl-loop
              for part in parts
              collect
              (list (cons (plist-get msg :id)
                          (plist-get part :id))
                    `[,(plist-get (plist-get msg :headers)
                                  :Subject)
                      ,(plist-get msg :date_relative)
                      ,(plist-get (plist-get msg :headers)
                                  :From)
                      ,(concat
                        (all-the-icons-icon-for-file (plist-get part :filename))
                        " "
                        (plist-get part :filename))])))))

(defun notmuch-list-attachments ()
  (interactive)
  (let ((query (notmuch-search-get-query))
        (buf (get-buffer-create "*Attachments*")))
    (with-current-buffer buf
      (notmuch-attachment-list-mode)
      (setq-local notmuch-attachment-list-query query)
      (tabulated-list-print)
      (goto-char (point-min))
      (hl-line-mode 1))
    (switch-to-buffer buf)))

