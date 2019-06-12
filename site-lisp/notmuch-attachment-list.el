;;; -*- lexical-binding: t; -*-

(require 'notmuch)
(require 'tabulated-list)
(require 'tabulated-list-utils)
(require 'gnus-dired)

(define-derived-mode notmuch-attachment-list-mode
  tabulated-list-mode
  "Attachment List"
  "Major mode for listing attachments"
  (setq tabulated-list-format
        `[("" 2 nil)
          ("File" 40 t)
          ("From" 20 t)
          ("Date" 15 t)
          ("Subject" 0 t)]
        
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
              (list `(:id
                      ,(plist-get msg :id)
                      :part
                      ,(plist-get part :id)
                      :filename
                      ,(plist-get part :filename))
                    `[,(all-the-icons-icon-for-file (plist-get part :filename)
 
                                                     )
                      ,(plist-get part :filename)
                      ,(plist-get (plist-get msg :headers)
                                  :From)
                      ,(plist-get msg :date_relative)
                      ,(plist-get (plist-get msg :headers)
                                  :Subject)
                      
                      ])))))


(defun notmuch-attachment-list-marked ()
  (let (marked)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (= (char-after) ?m)
          (push (tabulated-list-get-id) marked))
        (forward-line)))

    (or marked
        (list (tabulated-list-get-id)))))

(defun notmuch-attachment-list-save-attachment (attachment target-file)
  (let ((target-file (expand-file-name target-file)))
    (message "Save attachment to %s" target-file)
    (call-process "notmuch" nil `(:file ,target-file) nil
                  "show"
                  "--format=raw"
                  (format "--part=%d" (plist-get attachment :part))
                  "--body=true"
                  (format "id:%s" (plist-get attachment :id)))))

(defun notmuch-attachment-list-save (target)
  (interactive
   (list (read-file-name
          "Save to: "
          default-directory
          nil
	  nil nil
          (lambda (x)
            (and
             (file-directory-p x)
             (not (string-match-p (dired-omit-regexp) x)))))))

  (dolist (attachment (notmuch-attachment-list-marked))
    (notmuch-attachment-list-save-attachment
     attachment
     (concat target
             "/"
             (plist-get attachment :filename)))))

(defun notmuch-attachment-list-view-attachment (attachment)
  (let* ((tempfile (make-temp-file "attachment-" nil
                                   (plist-get attachment :filename))))
    (delete-file tempfile)
    (notmuch-attachment-list-save-attachment attachment tempfile)
    (let ((view-proc (start-process "view-attachment"
                                    nil
                                    "xdg-open"
                                    tempfile)))
      (set-process-sentinel
       view-proc
       (lambda (proc str)
         (when (string-match-p
                (rx bos (| "finished" "deleted" "exited" "failed"))
                str)
           (message "Deleted attachment %s" str tempfile)
           (delete-file tempfile)))))))

(defun notmuch-attachment-list-view ()
  (interactive)
  (dolist (attachment (notmuch-attachment-list-marked))
    (notmuch-attachment-list-view-attachment attachment)))

(defun notmuch-attachment-list-attach ()
  (interactive)
  (when-let ((attachments (notmuch-attachment-list-marked)))
    (let* ((message-buffers (gnus-dired-mail-buffers))
           
           (the-buffer (if message-buffers
                           (completing-read
                            "Attach to: "
                            (cons "New message"
                                  message-buffers))
                         "New message"))
           (the-buffer (if (string= "New message" the-buffer)
                           (progn (compose-mail)
                                  (current-buffer))
                         (get-buffer the-buffer)))

           (tempdir (make-temp-file "attachments" t)))

      (set-buffer the-buffer)
      (add-hook 'kill-buffer-hook
                (lambda ()
                  (delete-directory tempdir t))
                nil t)
      
      (goto-char (point-max))
      (dolist (attachment attachments)
        (let ((target (concat tempdir "/" (plist-get attachment :filename))))
          (notmuch-attachment-list-save-attachment attachment target)
          (mml-attach-file target (or (mm-default-file-encoding target)
                                      "application/octet-stream")))))))

(defun notmuch-attachment-list-mark ()
  (interactive)
  (tabulated-list-put-tag-region "m" t))

(defun notmuch-attachment-list-unmark ()
  (interactive)
  (tabulated-list-put-tag-region "" t))

(define-key notmuch-attachment-list-mode-map
  (kbd "s")
  'notmuch-attachment-list-save)

(define-key notmuch-attachment-list-mode-map
  (kbd "v")
  'notmuch-attachment-list-view)

;; attach to another message, warg
(define-key notmuch-attachment-list-mode-map
  (kbd "a")
  'notmuch-attachment-list-attach)

(define-key notmuch-attachment-list-mode-map
  (kbd "m")
  'notmuch-attachment-list-mark)

(define-key notmuch-attachment-list-mode-map
  (kbd "u")
  'notmuch-attachment-list-unmark)

(defun notmuch-list-attachments (query)
  (interactive
   (list (or (case major-mode
               (notmuch-search-mode (notmuch-search-get-query))
               (notmuch-show-mode (notmuch-show-get-query))
               (notmuch-tree-mode (notmuch-tree-get-query)))
             (notmuch-read-query "Query: "))))
  
  (let ((buf (get-buffer-create (concat "*notmuch-attachments-"
                                        query
                                        "*"))))
    (with-current-buffer buf
      (notmuch-attachment-list-mode)
      (setq-local notmuch-attachment-list-query query)
      (tabulated-list-print)
      (goto-char (point-min))
      (hl-line-mode 1))
    (switch-to-buffer buf)))

(provide 'notmuch-attachment-list)
