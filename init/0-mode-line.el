(defun mode-line-get-host ()
  (when (and (buffer-file-name)
             (file-remote-p (buffer-file-name)))
    (let ((parts (tramp-dissect-file-name (buffer-file-name))))
      (concat " "
              (tramp-file-name-user parts)
              "@"
              (tramp-file-name-host parts)
              ))))

(defvar mode-line-project nil)
(make-variable-buffer-local 'mode-line-project)

(defun mode-line-get-project ()
  (or mode-line-project
      (setq mode-line-project
            (or (when (and (buffer-file-name)
                           (featurep 'project)
                           (not (file-remote-p (buffer-file-name)))
                           (project-current nil))
                  (when-let ((n (project-current-name)))
                    (concat " ("
                            (propertize n
                                        'face 'underline
                                        'mouse-face 'mode-line-highlight
                                        'local-map (make-mode-line-mouse-map 'mouse-1 'project-dired))
                            ")")))
                ""
                ))))

(defun mode-line-pad-right (rhs)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (let* ((the-rhs (format-mode-line rhs))
         (reserve (length the-rhs)))
    (list
     (propertize " " 'display `((space :align-to (- right ,reserve)))) rhs))
  )


(setq-default
 frame-title-format
 '(dired-directory dired-directory "%b")
 
 mode-line-format
 `((:propertize "%5l" mouse-face mode-line-highlight)
   " %p %* "
   (:propertize "%b" mouse-face mode-line-highlight local-map
                ,(make-mode-line-mouse-map 'mouse-1 'dired-from-buffer))
   " %@"
   (:eval (mode-line-get-project))

   (vc-mode vc-mode)

   (:eval (mode-line-pad-right (list mode-line-misc-info " " mode-line-modes)))
   ))

;; (defvar mode-line-emacs-rss nil)
;; (defvar last-emacs-rss 0)
;; (defvar max-emacs-rss 0)

;; (defun check-emacs-resident-set-size ()
;;   (interactive)
;;   (let* ((default-directory user-emacs-directory)
;;          (rss (string-to-number
;;                (shell-command-to-string
;;                 (format "awk '/Rss:/{sum += $2} END {print sum}' /proc/%d/smaps " (emacs-pid)))))
;;          (delta (- rss last-emacs-rss)))
    
;;     (setq max-emacs-rss (max max-emacs-rss rss)
;;           last-emacs-rss rss)
    
;;     (setq mode-line-emacs-rss
;;           (concat
;;            (if (= max-emacs-rss last-emacs-rss) "*" "")
;;            (cond ((> delta 0) "+") ((= 0 delta) "=") (t "-"))
;;            (file-size-human-readable (* 1024 rss)))
;;           )))

;; (run-with-timer 1 10 #'check-emacs-resident-set-size)

;; (push
;;  `(mode-line-emacs-rss (:propertize mode-line-emacs-rss
;;                                     local-map
;;                                     ,(make-mode-line-mouse-map 'mouse-1 'check-emacs-resident-set-size)
;;                                     ))
;;  mode-line-misc-info)


(setq-default icon-title-format frame-title-format)

