(use-package org
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-refile-use-outline-path t
        org-goto-interface 'outline
        org-outline-path-complete-in-steps nil
        org-use-speed-commands t
        org-agenda-files '("~/notes/agenda")
        org-agenda-diary-file "~/notes/agenda/calendar.org"
        org-id-locations-file "~/notes/.metadata/org-id-locations"
        org-agenda-insert-diary-extract-time t
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
  )


(use-package org-extras
  :commands org-log-goto
  :bind ("C-c j" . org-log-goto)
  :config
  (setq org-log-location
  '("~/notes/journal/%Y/%B.org"
    "[%Y-%m-%d %a]")))

