(use-package htmlize :ensure t)

(use-package org
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-." . org-toggle-timestamp-type)
         ("M-p" . outline-previous-heading)
         ("M-n" . outline-next-heading)
         ("C-M-u" . outline-up-heading)
         ("C-M-f" . org-forward-heading-same-level)
         ("C-M-b" . org-backward-heading-same-level)
         ("C-M-d" . outline-next-heading)
         ("C-M-SPC" . org-mark-element)
         )
  :custom
  (org-refile-use-outline-path 'file)
  (org-goto-interface 'outline)
  (org-outline-path-complete-in-steps nil)
  (org-use-speed-commands t)
  (org-speed-commands-user
   '(("P" . org-set-property)))
  (org-agenda-files '("~/notes/agenda" "~/notes/work"))
  (org-agenda-todo-list-sublevels nil)
  (org-agenda-diary-file "~/notes/agenda/calendar.org")
  (org-id-locations-file "~/notes/.metadata/org-id-locations")
  (org-agenda-insert-diary-extract-time t)
  (org-adapt-indentation nil)
  (org-agenda-span 'week)
  (org-babel-load-languages '((emacs-lisp . t) (dot . t) (ditaa . t) (gnuplot . t)))
  (org-link-abbrev-alist
   '(("nhm" . "https://support.deccnhm.org.uk/thread/"))
   )
  (org-latex-pdf-process
   '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f"))
  (org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 2)))
  (org-refile-use-outline-path 'file)

  (org-capture-templates
   '(("p" "Person" entry (file"~/notes/work/people.org")
      "* %^{Name} %^g
%u")
     )
   )
  :config

  (major-mode-hydra-define
    org-mode (:quit-key "q")

    ("Display"
     (("i" org-toggle-inline-images "images")
      ("e" org-toggle-pretty-entities "entities")
      ("l" org-toggle-latex-fragment "latex"))

     "Insert"
     (("." org-time-stamp "<time>")
      (">" (org-time-stamp nil t) "[time]"))))

  (require 'org-notmuch-link)
  
  (with-eval-after-load 'org-agenda
    (require 'all-the-icons)
    (setq org-agenda-category-icon-alist
          `(("calendar" (display ,(all-the-icons-faicon "calendar" :height 0.75)))
            ("birthday" (display ,(all-the-icons-faicon "birthday-cake" :height 0.75)))
            ("holiday" (display ,(all-the-icons-faicon "calendar-o" :height 0.75)))
            ("work" (display ,(all-the-icons-octicon "briefcase" :height 0.75)))
            ("samba" (display ,(all-the-icons-faicon "child" :height 0.75))))))
  
  (defun org-read-date-analyze-no-stupid-endian (o ans org-def org-defdecode)
    (funcall o
             (if (string-match-p
                  "^ *\\(0?[1-9]\\|1[012]\\)/\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\(/\\([0-9]+\\)\\)?\\([^/0-9]\\|$\\)"
                  ans)
                 (replace-regexp-in-string "/" "." ans)
               ans)
             org-def org-defdecode))

  (advice-add 'org-read-date-analyze :around
              'org-read-date-analyze-no-stupid-endian)

  )

(with-eval-after-load 'org-export
  (defun org-odt-export-to-docx (&optional async subtreep visible-only ext-plist)
    (let ((org-odt-preferred-output-format "docx"))
      (org-odt-export-to-odt async subtreep visible-only ext-plist)))
  
  (org-export-define-derived-backend
      'docx 'odt
    :menu-entry
    '(?o "Export to ODT"
         ((?x "To docx file" org-odt-export-to-docx))

         ))


  ;; fix use of copy-file in ox-odt.el

  (defun org-odt-template-readonly-fix (o &rest args)
    (let ((orig-copy-file (symbol-function 'copy-file)))
      (cl-letf (((symbol-function 'copy-file)
                 (lambda (file newname &rest stuff)
                   (let ((out (apply orig-copy-file file newname stuff)))
                     (set-file-modes
                      newname
                      (logior (file-modes newname) #o600))
                     out))))
        (apply o args))))

  (advice-add 'org-odt-template :around 'org-odt-template-readonly-fix)
  )
  

(use-package org-extras
  :commands org-log-goto
  :bind ("C-c j" . org-log-goto)
  :defer 30
  :config
  (org-notify 1)
  (setq org-log-location
        '("~/notes/journal/%Y/W%W.org" "%a %d %b")))

;; UK public holidays, and other UK notable dates.
(setq calendar-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-new-year-bank-holiday)
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-fixed 3 17 "St. Patrick's Day")
        (holiday-fixed 4 1 "April Fools' Day")
        (holiday-easter-etc -47 "Shrove Tuesday")
        (holiday-easter-etc -21 "Mother's Day")
        (holiday-easter-etc -2 "Good Friday")
        (holiday-easter-etc 0 "Easter Sunday")
        (holiday-easter-etc 1 "Easter Monday")
        (holiday-float 5 1 1 "Early May Bank Holiday")
        (holiday-float 5 1 -1 "Spring Bank Holiday")
        (holiday-float 6 0 3 "Father's Day")
        (holiday-float 8 1 -1 "Summer Bank Holiday")
        (holiday-fixed 10 31 "Halloween")
        (holiday-fixed 12 24 "Christmas Eve")
        (holiday-fixed 12 25 "Christmas Day")
        (holiday-fixed 12 26 "Boxing Day")
        (holiday-christmas-bank-holidays)
        (holiday-fixed 12 31 "New Year's Eve")))
;; N.B. It is assumed that 1 January is defined with holiday-fixed -
;; this function only returns any extra bank holiday that is allocated
;; (if any) to compensate for New Year's Day falling on a weekend.
;;
;; Where 1 January falls on a weekend, the following Monday is a bank
;; holiday.
(defun holiday-new-year-bank-holiday ()
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y 1)
    (when (<= m 3)
      (let ((d (calendar-day-of-week (list 1 1 y))))
        (cond ((= d 6)
                (list (list (list 1 3 y)
                            "New Year's Day Bank Holiday")))
              ((= d 0)
                (list (list (list 1 2 y)
                            "New Year's Day Bank Holiday"))))))))

;; N.B. It is assumed that 25th and 26th are defined with holiday-fixed -
;; this function only returns any extra bank holiday(s) that are
;; allocated (if any) to compensate for Christmas Day and/or Boxing Day
;; falling on a weekend.
(defun holiday-christmas-bank-holidays ()
  (let ((m displayed-month)
        (y displayed-year))
    (calendar-increment-month m y -1)
    (when (>= m 10)
      (let ((d (calendar-day-of-week (list 12 25 y))))
        (cond ((= d 5)
                (list (list (list 12 28 y)
                            "Boxing Day Bank Holiday")))
              ((= d 6)
                (list (list (list 12 27 y)
                            "Boxing Day Bank Holiday")
                      (list (list 12 28 y)
                            "Christmas Day Bank Holiday")))
              ((= d 0)
                (list (list (list 12 27 y)
                            "Christmas Day Bank Holiday"))))))))

