(require 'tabulated-list)
(require 'projectile)

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
        `[("Name" 25 buffer-table-sort-filename)
          ("Group" 14 (lambda (a b) (buffer-table-sort-other 1 a b)))
          ("Mode" 12 t)
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
                        (match-string 1 name-a)))
         (name-b (progn (string-match (rx bos (? (: (+ any) "/")) (group (+ any)))
                                      name-b)
                        (match-string 1 name-b))))
    (string< name-a name-b)))

(defun buffer-table-sort-other (n b a)
  (let ((val-a (elt (cadr a) n))
        (val-b (elt (cadr b) n)))
    (cond ((string< val-a val-b) t)
          ((string= val-a val-b)
           (buffer-table-sort-filename b a))
          (t nil))))

(defun buffer-table-buffer-mode ()
  (let* ((n (symbol-name major-mode))
         (e (- (length n) 5)))
    (if (string= "-mode" (substring n e))
        (substring n 0 e)
      n)))

(defun buffer-table--get-buffers ()
  (cl-loop with e = nil
           for b being the buffers
           unless (string-match-p "^ " (buffer-name b))
           do (setq e
                    (with-current-buffer b
                      (let* ((buf-name (buffer-name))
                             (buf-path (or buffer-file-name (and dired-directory (expand-file-name dired-directory))))
                             (proj-name (and buf-path (projectile-project-name)))
                             (proj-root (and buf-path proj-name (projectile-project-root)))
                             (mode-name (buffer-table-buffer-mode))
                             (group (cond
                                     ((memq major-mode
                                            '(Custom-mode
                                              package-menu-mode
                                              messages-buffer-mode
                                              Buffer-menu-mode
                                              buffer-table-mode
                                              help-mode
                                              Info-mode
                                              ) ) "emacs")

                                     ((member buf-name
                                             '("*scratch*"
                                               "*Backtrace*"
                                               )
                                               )
                                      "emacs"
                                      )
                                     
                                     ((memq major-mode
                                            '(notmuch-show-mode
                                              notmuch-search-mode
                                              notmuch-message-mode))
                                      "email")

                                     ((memq major-mode
                                            '(rcirc-mode)
                                            )
                                      "irc")

                                     (proj-name proj-name)
                                     (t "")))
                             (buf-path
                              (if proj-root
                                  (substring buf-path (length (expand-file-name proj-root)))
                                (or buf-path ""))))
                        (list buf-name
                              `[,(propertize buf-name
                                             'face
                                             (cond
                                              (buffer-file-name 'bold)
                                              (dired-directory 'default)
                                              (t 'italic)))
                                ,group
                                ,mode-name
                                ,(or buf-path "")
                                ]))))
           and unless (buffer-table-filtered-p e)
           collect e))

(defun buffer-table ()
  (interactive)
  (let ((from (buffer-name (current-buffer)))
        (buf (get-buffer-create "*Buffer Table*")))
    (with-current-buffer buf
      (buffer-table-mode)
      (tabulated-list-print)
      (goto-char (point-min))
      (while (not (string= (tabulated-list-get-id)
                           from))
        (forward-line))
      (hl-line-mode 1))
    (switch-to-buffer buf)))

(defun buffer-table-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (tabulated-list-get-id)))

(define-key buffer-table-mode-map
  (kbd "RET")
  'buffer-table-switch-to-buffer)

(defun tabulated-list-put-tag-region (tag &optional next-line)
  (if (region-active-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (not (eobp))
            (tabulated-list-put-tag tag t))))
    (tabulated-list-put-tag tag next-line)))

(define-key buffer-table-mode-map
  (kbd "d")
  (lambda () (interactive)
    (tabulated-list-put-tag-region "d" t)))

(defun buffer-table-execute ()
  (interactive)
  (let ((delete-buffers nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (case (char-after)
          (?d (push (get-buffer (tabulated-list-get-id))
                    delete-buffers)))
        (forward-line)))
    (dolist (b delete-buffers)
      (kill-buffer b)))
  
  (tabulated-list-revert))

(define-key buffer-table-mode-map
  (kbd "x")
  'buffer-table-execute)

(define-key buffer-table-mode-map
  (kbd "s g")
  (lambda () (interactive) (tabulated-list-sort 1)))

(defun buffer-table-filter-like-this (n)
  (buffer-table-add-filter
   n (concat "^" (regexp-quote (elt (tabulated-list-get-entry) n)) "$")))

(define-key buffer-table-mode-map
  (kbd "= g")
  (lambda () (interactive)
    (buffer-table-filter-like-this 1)
    (tabulated-list-revert)))

(define-key buffer-table-mode-map
  (kbd "= =")
  (lambda () (interactive)
    (setq buffer-table-filters nil)
    (tabulated-list-revert)))

(provide 'buffer-table)
