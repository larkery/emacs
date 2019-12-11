(require 'org)
(require 'hydra)

(defvar org-agenda-event-hydra-date nil)
(defvar org-agenda-event-hydra-time nil)
(defvar org-agenda-event-hydra-location nil)
(defvar org-agenda-event-hydra-summary nil)
(defvar org-agenda-event-hydra-all-day nil)

(defhydra org-agenda-event-hydra
  (:hint nil)
  "
_d_ate: %s(format-time-string (if org-agenda-event-hydra-all-day \"%a %d %b %Y\" \"%a %d %b %Y %H:%M\") org-agenda-event-hydra-date) _s_: %s`org-agenda-event-hydra-summary
"
  ("d" (setq org-agenda-event-hydra-date (org-read-date t t nil "Start date: " nil)))
  ("D" (setq org-agenda-event-hydra-all-day (not org-agenda-event-hydra-all-day)))
  ("l" (setq org-agenda-event-hydra-location
             (read-string "Location: " org-agenda-event-hydra-location)))
  ("s" (setq org-agenda-event-hydra-summary
             (read-string "Summary: " org-agenda-event-hydra-summary)))
  ("t" (setq org-agenda-event-hydra-target
             (completing-read "Target: " (org-agenda-files) nil t org-agenda-event-hydra-target)))

  ("RET" org-agenda-event-hydra-finish)
  )

(defun org-agenda-event-hydra-finish ()
  (interactive)

  (let ((org-capture-templates
         `(("i" "Insert this thing" entry
            (file ,org-agenda-event-hydra-target)
            ,(s-lex-format "* ${org-agenda-event-hydra-summary}
:PROPERTIES:
:END:"
                     org-agenda-event-hydra-summary
                     
                     )
            ))
         )))
  )

(defun org-agenda-event-hydra-start (summary)
  (interactive "sSummary: ")

  (setq org-agenda-event-hydra-summary summary
        org-agenda-event-hydra-location nil
        org-agenda-event-hydra-date (org-get-cursor-date t)
        org-agenda-event-hydra-target (car (org-agenda-files)))
  
  (org-agenda-event-hydra/body))


(bind-key "i" #'org-agenda-event-hydra-start org-agenda-mode-map)

(provide 'org-agenda-event-hydra)
