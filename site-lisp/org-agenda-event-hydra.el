(require 'org)
(require 'hydra)

(defvar org-agenda-event-hydra-date nil)
(defvar org-agenda-event-hydra-time nil)
(defvar org-agenda-event-hydra-location nil)
(defvar org-agenda-event-hydra-summary nil)
(defvar org-agenda-event-hydra-all-day nil)
(defvar org-agenda-event-hydra-target nil)

(defhydra org-agenda-event-hydra
  (:hint nil)
  "
_s_: %s`org-agenda-event-hydra-summary, _l_oc: %s`org-agenda-event-hydra-location
_d_ate: %s(format-time-string \"%a %d %b %Y\" org-agenda-event-hydra-date) _t_ime: %s(if org-agenda-event-hydra-all-day \"All day\" (format-time-string \"%H:%M\" org-agenda-event-hydra-date))
_c_al: %s`org-agenda-event-hydra-target
"
  ("d" (setq org-agenda-event-hydra-date (org-read-date t t nil "Start date: " nil)))
  ("t" (if org-agenda-event-hydra-all-day
           (let* ((new-time
                   (read-string "Start time: "))
                  
                  (new-time (parse-time-string new-time))
                  (min (nth 1 new-time))
                  (hr (nth 2 new-time)))
             (when (and min hr)
               (setq
                org-agenda-event-hydra-all-day nil
                org-agenda-event-hydra-date
                (apply
                 #'encode-time
                 0 min hr
                 (nthcdr 3 (decode-time org-agenda-event-hydra-date))))))
         (setq org-agenda-event-hydra-all-day t)))
  
  ("l" (setq org-agenda-event-hydra-location
             (read-string "Location: " org-agenda-event-hydra-location)))
  ("s" (setq org-agenda-event-hydra-summary
             (read-string "Summary: " org-agenda-event-hydra-summary)))
  ("c" (let ((oaf (org-agenda-files)))
         (setq org-agenda-event-hydra-target
               (or
                (cadr (member org-agenda-event-hydra-target oaf))
                (car oaf)))))
  ("RET" org-agenda-event-hydra-finish :exit t))

(defun org-agenda-event-hydra-finish ()
  (interactive)

  (let* ((location
          (if org-agenda-event-hydra-location
              (format ":LOCATION: %s" org-agenda-event-hydra-location)
            ""))

         (formatted-date
          (format-time-string (if org-agenda-event-hydra-all-day
                                  "%Y-%m-%d %a"
                                "%Y-%m-%d %a %H:%M")
                    org-agenda-event-hydra-date))
         
         (org-capture-templates
          `(("i" "Insert this thing" entry
             (file ,org-agenda-event-hydra-target)
             ,(s-lex-format "* ${org-agenda-event-hydra-summary}
:PROPERTIES:
${location}
:END:
<${formatted-date}>
"
                            )))))
    (org-capture nil "i")
    ))

(defun org-agenda-event-hydra-start (summary)
  (interactive "sSummary: ")

  (let* ((summary "12:50 March for health @ wills building")
         (match (string-match
                (rx bos (? (group digit digit) (? ":" (group digit digit)) " ")
                    (group (*? (not (any "@"))))
                    (? (? " ") "@" (? " ") (group (* anything)))
                    eos)
                summary
                )))
    (match-string 3 summary))
  
  (let* ((match (string-match
                 (rx bos (? (group digit digit) (? ":" (group digit digit)) " ")
                     (group (*? (not (any "@"))))
                     (? (? " ") "@" (? " ") (group (* anything)))
                     eos)
                 summary))
         (date (org-get-cursor-date t))
         (hh (when (match-string 1 summary)
               (string-to-number (match-string 1 summary))))
         (mm (when (match-string 2 summary)
               (string-to-number (match-string 2 summary))))
         (date
          (if (or hh mm)
              (apply
               #'encode-time
               0 (or mm 0) (or hh 0)
               (nthcdr 3 (decode-time date)))
            date)))
    
    (setq org-agenda-event-hydra-summary (match-string 3 summary)
          org-agenda-event-hydra-location (match-string 4 summary)
          org-agenda-event-hydra-date date
          org-agenda-event-hydra-target (car (org-agenda-files))))
  
  (org-agenda-event-hydra/body))


(bind-key "i" #'org-agenda-event-hydra-start org-agenda-mode-map)

(provide 'org-agenda-event-hydra)
