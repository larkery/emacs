(use-package org
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("M-p" . outline-previous-heading)
         ("M-n" . outline-next-heading))
  
  :config
  
  (setq org-refile-use-outline-path t
        org-goto-interface 'outline
        org-outline-path-complete-in-steps nil
        org-use-speed-commands t
        org-speed-commands-user
        '(("P" . org-set-property))
        org-agenda-files '("~/notes/agenda")
        org-agenda-diary-file "~/notes/agenda/calendar.org"
        org-id-locations-file "~/notes/.metadata/org-id-locations"
        org-agenda-insert-diary-extract-time t
        org-adapt-indentation nil
        
        org-babel-load-languages '((emacs-lisp . t) (dot . t) (ditaa . t))

        org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f" "xelatex -interaction nonstopmode -output-directory %o %f")
        org-refile-targets '((nil . (:maxlevel . 3))))

  (defun org-refile-to-datetree ()
    (interactive)
    (let* ((ts (org-entry-get nil "TIMESTAMP"))
           (date (org-date-to-gregorian ts)))
      
      (save-excursion
        (org-datetree-find-date-create date))
      (save-excursion
        (org-mark-subtree)
        (org-datetree-file-entry-under
         (buffer-substring (region-beginning) (region-end))
         date))
      (org-cut-subtree)))


  (defun org-read-date-analyze-no-stupid-endian (o ans org-def org-defdecode)
    (funcall o
             (if (string-match-p
                  "^ *\\(0?[1-9]\\|1[012]\\)/\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)\\(/\\([0-9]+\\)\\)?\\([^/0-9]\\|$\\)"
                  ans)

                 (replace-regexp-in-string "/" "." ans)

                 ans)

             org-def org-defdecode)
    )

  (advice-add 'org-read-date-analyze :around
              'org-read-date-analyze-no-stupid-endian)
  
  )


(use-package org-extras
  :commands org-log-goto
  :bind ("C-c j" . org-log-goto)
  :config
  (setq org-log-location
  '("~/notes/journal/%Y/%B.org"
    "[%Y-%m-%d %a]")))

(use-package date-at-point
  :ensure t
  
  :config
  (defun org-agenda-date-at-point (o &rest args)
    (let* ((dap (date-at-point))
           (org-agenda-start-day
            (or org-agenda-start-day
                dap)))
      (apply o args)))

  (advice-add 'org-agenda :around 'org-agenda-date-at-point)
  
  )
