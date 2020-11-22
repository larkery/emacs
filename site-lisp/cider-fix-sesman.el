;; patch cider-repl-create to force reuse of existing sesman session?

;; cider--check-existing-session asks about this

(defun cider-repl-create-force-session (args)
  (let* ((params (car args))
         (override (plist-get params :force-session-name)))
    (if override
        (progn
          (message "override session name: %s" override)
          (plist-put params :session-name override)
          (list params))
      args)))

(advice-add 'cider-repl-create :filter-args 'cider-repl-create-force-session)

(defun cider--check-existing-session-hack (params)
  "Ask for confirmation if a session with similar PARAMS already exists.
If no session exists or user chose to proceed, return PARAMS.  If the user
canceled the action, signal quit."
  (let* ((proj-dir (plist-get params :project-dir))
         (host (plist-get params :host))
         (port (plist-get params :port))
         (session (seq-find (lambda (ses)
                              (let ((ses-params (cider--gather-session-params ses)))
                                (and (equal proj-dir (plist-get ses-params :project-dir))
                                     (or (null port)
                                         (equal port (plist-get ses-params :port)))
                                     (or (null host)
                                         (equal host (plist-get ses-params :host))))))
                            (sesman-current-sessions 'CIDER '(project)))))
    (when session
      (let ((c (read-char-from-minibuffer
                (concat "Session exists (" (car session) "). "
                        "[Q]uit, [N]ew session, or Hack [S]ibling: "))))

        (case c
          ((?q ?Q)
           (let ((debug-on-quit nil)) (signal 'quit nil)))
          ((?n ?N) params)
          ((?s ?S)
           (plist-put params :force-session-name (car session)))))))
  params)

(advice-add 'cider--check-existing-session :override #'cider--check-existing-session-hack)

(provide 'cider-fix-sesman)
