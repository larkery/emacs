;; this fails because autoloading does not require but does provide.
;; I could advise provide, but a feature can be in many files.
;; there is package-alist instead
;; this says path -> package

(require 'cl)

(defun defcustom-record-load-file (symbol &rest args)
  ;; (let ((a nil))
  ;;   (mapbacktrace (lambda (evald func args flags) (push func a)))
  ;;   (message "%s %s" symbol a))

  ;; (push (cons symbol load-file-name) defcustom-load-file)
  )

(advice-add 'custom-declare-variable :before #'defcustom-record-load-file)

(defvar use-package-locations nil)

(defun use-package-record-location (sym &rest args)
  (push (cons sym load-file-name) use-package-locations))

(advice-add 'use-package :before 'use-package-record-location)

(defun use-package-format-custom-for (symbols &optional quoted)
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (dolist (symbol symbols)
        (let ((spec (car-safe (get symbol 'theme-value)))
	      (value (get symbol 'saved-value))
	      (requests (get symbol 'custom-requests))
	      (now (and (not (custom-variable-p symbol))
		        (or (boundp symbol)
			    (eq (get symbol 'force-value)
			        'rogue))))
	      (comment (get symbol 'saved-variable-comment)))
          ;; Check REQUESTS for validity.
          (dolist (request requests)
	    (when (and (symbolp request) (not (featurep request)))
	      (message "Unknown requested feature: %s" request)
	      (setq requests (delq request requests))))
          ;; Is there anything customized about this variable?
          (when (or (and spec (eq (car spec) 'user))
		    comment
		    (and (null spec) (get symbol 'saved-value)))
	    ;; Output an element for this variable.
	    ;; It has the form (SYMBOL VALUE-FORM NOW REQUESTS COMMENT).
	    ;; SYMBOL is the variable name.
	    ;; VALUE-FORM is an expression to return the customized value.
	    ;; NOW if non-nil means always set the variable immediately
	    ;; when the customizations are reloaded.  This is used
	    ;; for rogue variables
	    ;; REQUESTS is a list of packages to load before setting the
	    ;; variable.  Each element of it will be passed to `require'.
	    ;; COMMENT is whatever comment the user has specified
	    ;; with the customize facility.
	    (unless (bolp)
	      (princ "\n"))
	    (if quoted
                (princ "  '(")
                (princ "  ("))
	    (prin1 symbol)
	    (princ " ")
	    (let ((val (prin1-to-string (car value))))
	      (if (< (length val) 60)
	          (insert val)
	        (newline-and-indent)
	        (let ((beginning-of-val (point)))
	          (insert val)
	          (save-excursion
		    (goto-char beginning-of-val)
		    (indent-pp-sexp 1)))))
	    (when (or now requests comment)
	      (princ " ")
	      (prin1 now)
	      (when (or requests comment)
	        (princ " ")
	        (prin1 requests)
	        (when comment
	          (princ " ")
	          (prin1 comment))))
	    (princ ")"))))
      (buffer-string))))

(defun use-package-save-custom-for (symbols)
  "Assuming the current buffer is narrowed to a use-package
statement, write a :custom for it."

  (let ((custom-block (use-package-format-custom-for symbols)))
    (if (search-forward-regexp ":custom\\(\\s-+\\|\n\\|(\\)" nil t)
        (let ((custom-start (point)))
          (while (not (or (eobp)
                          (looking-at
                           "\\()\\|\\(\\s-\\|\n\\)*:[a-z-]+\\)")))
            (forward-sexp))
          (delete-region custom-start (point)))
      (goto-char (point-min))
      (forward-sexp)
      (backward-char)
      (insert ":custom\n"))
    (insert custom-block)))

;; (defun use-package-package-for-custom-variable (symbol)
;;   (let ((file (alist-get symbol custom-load-file)))
;;     (when file
;;       (cl-loop
;;        for package in package-alist
;;        ;; so we can ask for the install directory of a package
;;        ;; but built-in packages have no install directory
;;        ;; so this also doesn't cut it. ARGH
;;        unless (package-)
;;        )
;;       )
;;     )
;;   )


(defun use-package-save-custom ()
  (interactive)
  (let ((saved-list (make-list 0 1))
        (remaining-list (make-list 0 1)))
    (mapatoms
     (lambda (symbol)
       (when (and (get symbol 'saved-value)
	          ;; ignore theme values
	          (or (null (get symbol 'theme-value))
		      (eq 'user (caar (get symbol 'theme-value)))))
         (let* ((symbol-feature (alist-get symbol feature-by-custom-variable))
                (feature-symbols (alist-get symbol-feature saved-list)))
           (if feature-symbols
               (push symbol (cdr feature-symbols))
             (push (list symbol-feature symbol) saved-list))))))
    ;; now we have symbols to save grouped by feature they are sort of inside.
    
    (dolist (feature-symbols saved-list)
      (let* ((feature (car feature-symbols))
             (symbols (cdr feature-symbols))
             (init-file (alist-get feature use-package-locations))
             ;; if we didn't get an init-file this way, can we find
             ;; one with a prefix?
             (init-file (or init-file
                            (and feature
                                 (cl-loop
                                  for cell in use-package-locations
                                  until (string-match-p
                                         (concat"^" (regexp-quote (symbol-name (car cell))))
                                         (symbol-name feature))
                                  return (cdr cell))))))
        (message "init file for %s: %s" feature init-file)
        (unless (and init-file
                     (with-current-buffer (find-file-noselect init-file t)
                       (when (search-forward-regexp
                              (concat "(use-package[[:blank:]]+" (regexp-quote (symbol-name feature)))
                              nil t)
                         (save-restriction
                           (narrow-to-defun)
                           (goto-char (point-min))
                           (use-package-save-custom-for symbols)
                           t))))
          ;; this is the fallback for symbols that didn't have a place to go
          (setq remaining-list (nconc remaining-list symbols)))))
    (with-current-buffer (find-file-noselect custom-file t)
      (erase-buffer)
      (insert "(custom-set-variables\n")
      (insert (use-package-format-custom-for remaining-list t))
      (insert ")")
      )
    ))

(provide 'use-package-save-custom)
