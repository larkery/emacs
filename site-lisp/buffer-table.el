(require 'tabulated-list)
(require 'tabulated-list-utils)
(require 'project)

(defvar buffer-table-group-col 2)
;; (defvar buffer-table-mode-col 3)
(defvar buffer-table-host-col 3)

(defvar buffer-table-delete-mark (propertize "d" 'face 'error))
(defvar buffer-table-save-mark (propertize "s" 'face 'success))

(defvar buffer-table-filters nil)
(make-variable-buffer-local 'buffer-table-filters)

(defun buffer-table-add-filter (col re)
  (push (cons col re) buffer-table-filters))

(defun buffer-table-filtered-p (entry)
  (when buffer-table-filters
    (let ((vals (cadr entry))
          (filters buffer-table-filters)
          result)
      (while filters
        (let* ((f (pop filters))
               (val (elt vals (car f))))
          (unless (string-match-p (cdr f) val)
            (setq filters nil result t))))
      result)))

(define-derived-mode buffer-table-mode tabulated-list-mode "Buffer Table"
  "Major mode for listing buffers"
  (setq tabulated-list-format
        `[("%" 2 t)
          ("Name" 30 buffer-table-sort-filename)
          ("Group" 14 (lambda (a b) (buffer-table-sort-other buffer-table-group-col a b)))
          ;; ("Mode" 12 t)
          ("Host" 8 t)
          ("Filename" 0 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries #'buffer-table--get-buffers)
  (tabulated-list-init-header))

(defun buffer-table-sort-filename (b a)
  (let* ((name-a (car a))
         (name-b (car b))
         (name-a (progn (string-match (rx bos (? (: (+ any) "/"))  (group (+ any)))
                                      name-a)
                        (match-string buffer-table-group-col name-a)))
         (name-b (progn (string-match (rx bos (? (: (+ any) "/")) (group (+ any)))
                                      name-b)
                        (match-string buffer-table-group-col name-b))))
    (string< name-a name-b)))

(defun buffer-table-sort-other (n b a)
  (let ((val-a (elt (cadr a) n))
        (val-b (elt (cadr b) n)))
    (cond ((string< val-a val-b) t)
          ((string= val-a val-b)
           (buffer-table-sort-filename b a))
          (t nil))))

;; (defun buffer-table-buffer-mode ()
;;   (let* ((n (symbol-name major-mode))
;;          (e (- (length n) 5)))
;;     (if (string= "-mode" (substring n e))
;;         (substring n 0 e)
;;       n)))


(defun buffer-table--get-buffers ()
  (cl-loop with e = nil
           for b being the buffers
           unless (string-match-p "^ " (buffer-name b))
           do (setq e
                    (with-current-buffer b
                      (let* ((buf-name (buffer-name))
                             (buf-path (or buffer-file-name (and dired-directory (expand-file-name dired-directory))))
                             (remote (and buf-path (file-remote-p buf-path)))
                             (proj-name (and (not remote) (project-current-name)))
                             (proj-root (and buf-path proj-name (project-current-root)))
                             ;; (mode-name (buffer-table-buffer-mode))
                             (host-name
                              (or (and buf-path
                                       remote
                                       (tramp-file-name-host-port
                                        (tramp-dissect-file-name buf-path)))
                                  ""))
                             
                             (group (cond
                                     ((memq major-mode
                                            '(Custom-mode
                                              package-menu-mode
                                              messages-buffer-mode
                                              Buffer-menu-mode
                                              buffer-table-mode
                                              help-mode
                                              Info-mode
                                              bookmark-bmenu-mode
                                              ) ) "*")

                                     ((member buf-name
                                              '("*scratch*"
                                                "*Backtrace*"
                                                )
                                              )
                                      "*"
                                      )
                                     
                                     ((memq major-mode
                                            '(notmuch-show-mode
                                              notmuch-tree-mode
                                              notmuch-search-mode
                                              notmuch-message-mode
                                              notmuch-attachment-list-mode
                                              ))
                                      "mail")

                                     (proj-name proj-name)
                                     (t "")))
                             (buf-path
                              (if proj-root
                                  (let ((r (substring buf-path (length (expand-file-name proj-root)))))
                                    (if (string= r "/")
                                        buf-path
                                      r))
                                (or buf-path ""))))
                        (list buf-name
                              `[,(cond
                                  ((buffer-modified-p) "%")
                                  (t ""))
                                
                                ,(concat
                                  (let ((n (or (and (buffer-file-name)
                                                    (all-the-icons-icon-for-file
                                                     (file-name-nondirectory (buffer-file-name))
                                                     :height 0.75))
                                               (all-the-icons-icon-for-mode
                                                major-mode
                                                :height 0.75)
                                               )))
                                    (if (stringp n)
                                        n (all-the-icons-octicon
                                           "gear" :height 0.75)))
                                  
                                  
                                  " "
                                  (propertize buf-name
                                              'face
                                              (cond
                                               (buffer-file-name 'bold)
                                               (dired-directory 'default)
                                               (t 'italic))))
                                ,group
                                ;; ,mode-name
                                ,host-name
                                ,(or buf-path "")
                                ]))))
           and unless (buffer-table-filtered-p e)
           collect e))

(defun buffer-table ()
  (interactive)
  (let ((from (buffer-name (current-buffer)))
        (buf (get-buffer-create "*Buffer Table*")))
    (with-current-buffer buf
      (if (eq major-mode 'buffer-table-mode)
          (progn
            (tabulated-list-revert)
            (unless (tabulated-list-goto-id from)
              (setq buffer-table-filters nil)
              (tabulated-list-revert)
              (tabulated-list-goto-id from)))
        
        (buffer-table-mode)
        (tabulated-list-print)
        (goto-char (point-min))
        (while (not (string= (tabulated-list-get-id)
                             from))
          (forward-line))
        (hl-line-mode buffer-table-group-col)))
    (let ((windows (get-buffer-window-list buf)))
      (if windows
          (select-window (car windows))
        (switch-to-buffer buf)))))

(defun buffer-table-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (tabulated-list-get-id)))

(define-key buffer-table-mode-map
  (kbd "RET")
  'buffer-table-switch-to-buffer)

(defun tabulated-list-put-tag-similar (group tag)
  (let ((this-group (elt (tabulated-list-get-entry) group)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (string= (elt (tabulated-list-get-entry) group) this-group)
            (tabulated-list-put-tag tag t)
          (forward-line))))))

(defun buffer-table-mark-kill ()
  (interactive)
  (tabulated-list-put-tag-region buffer-table-delete-mark t))

(defun buffer-table-mark-save ()
  (interactive)
  (tabulated-list-put-tag-region buffer-table-save-mark t))

(define-key buffer-table-mode-map
  (kbd "d") 'buffer-table-mark-kill)

(define-key buffer-table-mode-map
  (kbd "s") 'buffer-table-mark-save)

(defun buffer-table-kill-group ()
  (interactive)
  (tabulated-list-put-tag-similar buffer-table-group-col buffer-table-delete-mark))

(defun buffer-table-save-group ()
  (interactive)
  (tabulated-list-put-tag-similar buffer-table-group-col buffer-table-save-mark))

(define-key buffer-table-mode-map
  (kbd "D g")
  'buffer-table-kill-group)

(define-key buffer-table-mode-map
  (kbd "S g")
  'buffer-table-save-group)

;; (defun buffer-table-kill-mode ()
;;   (interactive)
;;   (tabulated-list-put-tag-similar buffer-table-mode-col buffer-table-delete-mark))

;; (define-key buffer-table-mode-map
;;   (kbd "D m")
;;   'buffer-table-kill-mode)


(defun buffer-table-unmark-all ()
  (interactive)
  (tabulated-list-put-tag-all ""))

(define-key buffer-table-mode-map
  (kbd "U")
  'buffer-table-unmark-all)

(defun buffer-table-unmark ()
  (interactive)
  (tabulated-list-put-tag "" t))

(define-key buffer-table-mode-map
  (kbd "u") 'buffer-table-unmark)

(defun buffer-table-execute ()
  (interactive)
  (let ((delete-buffers nil)
        (save-buffers nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cl-case (char-after)
          (?d (push (get-buffer (tabulated-list-get-id))
                    delete-buffers))
          (?s (push (get-buffer (tabulated-list-get-id))
                    save-buffers)))
        (forward-line)))
    (dolist (b delete-buffers)
      (kill-buffer b))
    
    (dolist (b save-buffers)
      (with-current-buffer b (save-buffer))))
  
  (tabulated-list-revert))

(define-key buffer-table-mode-map
  (kbd "x")
  'buffer-table-execute)

(defun buffer-table-sort-by-group ()
  (interactive) (tabulated-list-sort buffer-table-group-col))

(define-key buffer-table-mode-map
  (kbd "o g")
  'buffer-table-sort-by-group)

(defun buffer-table-filter-like-this (n)
  (buffer-table-add-filter
   n (concat "^" (regexp-quote (elt (tabulated-list-get-entry) n)) "$")))

(defun buffer-table-filter-group ()
  (interactive)
  (buffer-table-filter-like-this buffer-table-group-col)
  (tabulated-list-revert))

(defun buffer-table-filter-host ()
  (interactive)
  (buffer-table-filter-like-this buffer-table-host-col)
  (tabulated-list-revert))

(define-key buffer-table-mode-map
  (kbd "f g")
  'buffer-table-filter-group)

(define-key buffer-table-mode-map
  (kbd "f h")
  'buffer-table-filter-host)

(defun buffer-table-un-filter ()
  (interactive)
  (setq buffer-table-filters nil)
  (tabulated-list-revert))

(define-key buffer-table-mode-map
  (kbd "f f")
  'buffer-table-un-filter)

;; (pretty-hydra-define
;;   buffer-table
;;   (:quit-key ("q") :title "Buffer List" :foreign-keys run)
;;   ("Sort"
;;    (("o g" buffer-table-sort-by-group "group"))
   
;;    "Filter"
;;    (("f g" buffer-table-filter-group "group")
;;     ("f h" buffer-table-filter-host "host")
;;     ("f f" buffer-table-un-filter "none"))

;;    "Kill"
;;    (("D g" buffer-table-kill-group "group")
;;     ("D m" buffer-table-kill-mode "mode"))

;;    "Act"
;;    (("U" buffer-table-unmark-all "clear marks")
;;     ("x" buffer-table-execute "execute"))
;;    )

  
;;   )


(define-key buffer-table-mode-map
  (kbd "SPC")
  'buffer-table/body
  )

(provide 'buffer-table)
