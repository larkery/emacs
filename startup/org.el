(use-package org
  :bind (("C-c a" . org-agenda))
  :config
  (setq org-refile-use-outline-path 'file
        org-goto-interface 'outline-path-completion
        org-outline-path-complete-in-steps nil
        org-use-speed-commands t
        org-agenda-diary-file "~/notes/agenda/calendar.org"
        org-id-locations-file "~/notes/.metadata/org-id-locations"
        )
  )
