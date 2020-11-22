(require 'tabulated-list)
(require 'cl)
(require 'json)
(require 'tabulated-list-utils)

(defvar contacts-file "~/notes/contacts.json")
(defvar contacts-data nil)

(define-derived-mode contacts-mode tabulated-list-mode
  "Contacts" "A contact list"

  (setq tabulated-list-format
        `[("Name"   30 t)
          ("Tags" 15 t)
          ("Org" 6 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key '("Name")
        tabulated-list-entries #'contacts-mode-rows
        contacts-data (or contacts-data
                          (json-read-file contacts-file)))
  (tabulated-list-init-header))

(defun contact-name (c)
  (let ((giv (alist-get 'firstName c))
        (mid (alist-get 'middleName c))
        (sur (alist-get 'surname c)))
    (concat giv
            (and giv " ")
            mid
            (and mid " ")
            sur)))

(defun contact-first-name (c)
  (or (alist-get 'firstName c) ""))

(defun contact-family-name (c)
  (or (alist-get 'surname c) ""))

(defun contact-organisation (c)
  (or (alist-get 'organisation c) ""))

(defun contact-email (c)
  ;; any email will do
  (let ((es (alist-get 'email c)))
    (if es
        (let ((e (elt es 0)))
          (elt e 1))
      
      "")))

(defun contact-tel (c)
  (let ((es (alist-get 'phone c)))
    (if es
        (let ((e (elt es 0)))
          (elt e 1))
      
      "")))

(defun contact-tags (c)
  (let ((es (alist-get 'groups c)))
    (if es (mapconcat 'identity es ",") "")))

(defun contact-address (c)
  (let ((es (alist-get 'address c)))
    (replace-regexp-in-string
     (regexp-quote "\n") ", "
     (if es
         (let ((e (elt es 0)))
           (elt e 1))
       
       ""))))

(defun contacts-current-entry ()
  (alist-get
   (tabulated-list-get-id)
   contacts-data))

(defun contacts-compose ()
  (interactive)
  (let* ((ids (tabulated-list-marked-or-current-id ?m))
         out)
    
    (dolist (id ids)
      (let* ((c (alist-get id contacts-data))
             (es (alist-get 'email c))
             (e (cond
                 ((= 1 (length es)) (elt (elt es 0) 1))
                 (es
                  (let ((e (completing-read
                            "Address: "
                            (cl-loop for e across es
                                     collect (format "%s: %s" (elt e 0) (elt e 1)))
                            nil t)))
                    (when e
                      (substring e (+ 2 (position ?: e)))))))))
        (when e (push e out))))

    (when out
      (contacts-unmark-all)
      (compose-mail (mapconcat 'identity out ", ")))))

(defun contacts-mode-rows ()
  (cl-loop for contact in contacts-data
           collect
           (list
            (car contact)
            (let ((p (cdr contact)))
              (vector
               (contact-name p)
               (contact-tags p)
               (contact-organisation p)
               )))))

(defun contacts-save ()
  (interactive)
  (with-temp-buffer
    (insert (json-encode contacts-data))
    (write-file contacts-file)))

(defun contacts-revert ()
  (interactive)
  (setq contacts-data (json-read-file contacts-file))
  (tabulated-list-revert))

(defun contacts-edit ()
  (interactive)
  (contact-edit (tabulated-list-get-id)))

(defun contacts-mark ()
  (interactive)
  (tabulated-list-put-tag-region "m" t))

(defun contacts-unmark ()
  (interactive)
  (tabulated-list-put-tag-region " " t))

(defun contacts-unmark-all ()
  (interactive)
  (tabulated-list-put-tag-all " "))

(defun contacts-merge-groups (groups add remove)
  (seq-into
   (remove-duplicates
    
    )
   'vector))


(defun contacts-edit-groups ()
  (interactive)
  (let* ((ids (tabulated-list-marked-or-current-id ?m))
         (all-gs  (seq-into
                   (remove-duplicates
                    (cl-loop
                     for cell in contacts-data
                     vconcat (alist-get 'groups (cdr cell)))
                    :test #'equal)
                   'list))

         (gs (vconcat (remove-duplicates
                       (completing-read-multiple
                        "Groups: "
                        (append all-gs (cl-loop for x in all-gs collect (concat "-" x))))
                       :test #'equal)))
         (add )
         )
    (dolist (id ids)
      (cl-callf #'contacts-merge-groups
          (alist-get 'groups (alist-get id contacts-data))
        add remove)))
  (tabulated-list-revert))

(define-key contacts-mode-map (kbd "m") #'contacts-mark)
(define-key contacts-mode-map (kbd "U") #'contacts-unmark-all)
(define-key contacts-mode-map (kbd "u") #'contacts-unmark)
(define-key contacts-mode-map (kbd "e") #'contacts-compose)
(define-key contacts-mode-map (kbd "g") #'contacts-revert)
(define-key contacts-mode-map (kbd "+") #'contacts-edit-groups)
(define-key contacts-mode-map (kbd "C-x C-s") #'contacts-save)
;(define-key contacts-mode-map (kbd "so") #'contact-set-org)
(define-key contacts-mode-map (kbd "RET") #'contacts-edit)

(defun contacts-buffer () (get-buffer-create "*Contacts*"))

(defun contacts ()
  (interactive)
  (let ((b (contacts-buffer)))
    (with-current-buffer b
      (contacts-mode)
      (tabulated-list-print)
      (goto-char (point-min))
      (hl-line-mode t))
    (switch-to-buffer b)))

(defvar current-contact-id nil)
(make-variable-buffer-local 'current-contact-id)

(define-derived-mode contact-mode tabulated-list-mode
  "Contact" "Editing one contact"

  (setq tabulated-list-format
        `[("Field" 3 t)
          ("Category", 8)
          ("Value" 0)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil
        tabulated-list-entries #'contact-mode-rows))

(defun contact-pad-field (a)
  (replace-regexp-in-string
   (regexp-quote "\n")
   "\n               "
   a))

(defun contact-mode-rows ()
  (let ((the-contact (alist-get current-contact-id contacts-data)))
    `(
      ,(list 'firstName `["fn" "" ,(or (alist-get 'firstName the-contact) "")])
      ,(list 'middleName `["mn" "" ,(or (alist-get 'middleName the-contact) "")])
      ,(list 'surname `["sn" "" ,(or (alist-get 'surname the-contact) "")])
      ,(list 'organisation `["org" "" ,(or (alist-get 'organisation the-contact) "")])
      ,(list 'birthday `["🧁" "" ,(or (alist-get 'birthday the-contact) "")])
      ,(list 'anniversary `["🟊" "" ,(or (alist-get 'anniversary the-contact) "")])

      ,@(cl-loop
         for email across (alist-get 'email the-contact)
         collect
         (list 'email `["@" ,(elt email 0) ,(elt email 1)]))

      ,@(cl-loop
         for phone across (alist-get 'phone the-contact)
         collect
         (list 'phone `["☎" ,(elt phone 0) ,(elt phone 1)]))

      ,@(cl-loop
         for addr across (alist-get 'address the-contact)
         collect
         (list 'address `["✉" ,(elt addr 0) ,(contact-pad-field (elt addr 1))]))

      ,@(cl-loop
         for group across (alist-get 'groups the-contact)
         collect
         (list 'group `["g" "" ,group])
         )
      
      ,(list 'notes `["?" "" ,(contact-pad-field (or (alist-get 'notes the-contact) ""))])
      )
    )
  )

(defun contact-edit (id)
  (let ((c (alist-get id contacts-data)))
    (let ((b (get-buffer-create
              (format "*%s*" (contact-name c)))))
      (with-current-buffer b
        (contact-mode)
        (goto-char (point-min))
        (hl-line-mode t)
        (setq current-contact-id id)
        (tabulated-list-print))
      (select-window
        (display-buffer b))
      )))



(provide 'contacts)
